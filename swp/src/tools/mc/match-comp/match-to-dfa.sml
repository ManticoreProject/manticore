(* match-to-dfa.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Translate complex match cases to DFA representation.
 *
 * FIXME: the simplified patterns now carry vmaps on pattern tuples.  We
 * need to push this information into the matrix representation and then
 * into the DFA arcs.  Perhaps we should change the type of matrix rows
 * to have an arc instead of a state?
 *)

structure MatchToDFA : sig

    val rulesToDFA : (Error.span * AST.var * AST.match list) -> MatchDFA.dfa

  end = struct

    structure L = Literal
    structure Ty = Types
    structure DFA = MatchDFA
    structure DC = DataCon
    structure VMap = Var.Map
    structure VSet = Var.Set

(*FIXME*)val dummy : DFA.var_map = VMap.empty

    val union = VMap.unionWithi
	  (fn (x, _ : DFA.path, _) => raise Fail("multiple occurrences of "^Var.toString x))


  (******************** Pattern matrix ********************)

  (* simplified source patterns (after variable renaming) *)
    datatype pat
      = P_Wild
      | P_Lit of (Literal.literal * Ty.ty)
      | P_Con of AST.dcon * Ty.ty list
      | P_ConApp of (AST.dcon * Ty.ty list * DFA.path * pat)
      | P_Tuple of (DFA.path * pat) list

    datatype cell
      = NIL
      | CELL of {
	  pat : pat,
	  right : cell,			(* the cell to the right in the matrix *)
	  down : cell			(* the cell to the left in the matrix *)
	}

  (* a row of cells in a pattern matrix *)
    datatype row = R of {
	  vmap : DFA.var_map,		(* variables bound to cells in this *)
					(* row, with their mapping to paths. *)
	  cells : cell,			(* cell of the first column *)
	  optCond : (VSet.set * AST.exp) option, (* optional "where" clause *)
	  act : DFA.state		(* corresponding action state *)
	}

    datatype matrix = M of {
	    rows : row list,
	    cols : cell list,		(* cells of the top row *)
	    vars : DFA.path list	(* variables being tested (one per *)
					(* column *)
	  }

    fun mkNilMat vars = M{rows = [], cols = List.map (fn _ => NIL) vars, vars = vars}

    fun rowToList NIL = []
      | rowToList (cell as CELL{right, ...}) = cell :: rowToList right

  (* create a pattern matrix from a list of rows.  THe matrix will be a column
   * vector, since there is a single pattern per row.
   *)
    fun mkMatrix (arg, match as (pat1, _, _, _)::_) = let
	  fun mkRows [] = (NIL, [])
	    | mkRows ((pat, optCond, vmap, q)::rows) = let
		val (topCell, rows) = mkRows rows
		val cell = CELL{pat = pat, right = NIL, down = topCell}
		val row = R{vmap = vmap, cells = cell, optCond=optCond, act=q}
		in
		  (cell, row::rows)
		end
	  val (topCell, rows) = mkRows match
	  in
	    M{ rows = rows, cols = [topCell], vars = [arg] }
	  end

  (* add a row to the top of a matrix *)
    fun addRow (M{rows, cols, vars}, R{vmap, cells, optCond, act}) = let
	  fun cons (NIL, []) = (NIL, [])
	    | cons (CELL{pat, right = r1, ...}, dn::r2) = let
		val (right, cols) = cons(r1, r2)
		val cell = CELL{pat = pat, right = right, down = dn}
		in
		  (cell, cell::cols)
		end
	  val (row, cols) = cons (cells, cols)
	  val r = R{vmap=vmap, cells=row, optCond=optCond, act=act}
	  in
	    M{rows = r :: rows, cols = cols, vars = vars}
	  end

  (* replace the ith variable in var with newVars *)
    fun expandVars (vars, i, newVars) = let
	  fun ins (0, _::r) = newVars @ r
	    | ins (i, v::r) = v :: ins(i-1, r)
	  in
	    ins (i, vars)
	  end

  (* replace the ith cell of a row with the expansion of args *)
    fun expandCols (R{vmap, cells, optCond, act}, i, args) = let
	  fun ins (0, CELL{right, ...}) = let
		fun cons [] = right
		  | cons ((_, pat)::r) = CELL{
			pat = pat, down = NIL, right = cons r
		      }
		in
		  cons args
		end
	    | ins (i, CELL{pat, right, ...}) = CELL{
		  pat = pat, down = NIL, right = ins (i-1, right)
		}
	  in
(* FIXME: we need to remove the variables bound in the ith column from vmap! *)
	    R{vmap = vmap, cells = ins (i, cells), optCond = optCond, act = act}
	  end


  (******************** Matrix splitting ********************)

    datatype coverage
      = ALL			(* all cases covered *)
      | PARTIAL			(* partial coverage *)

(*+DEBUG*)
    fun coverToString ALL = "exhaustive"
      | coverToString PARTIAL = "nonexhaustive"
(*-DEBUG*)

    local
      structure S = IntRedBlackSet
      fun add cvt (s, item) = S.add(s, cvt item)
    in
  (* return the coverage of a list of patterns. *)
    fun coverage pats = let
	  fun chkForAny l =
		if (List.exists (fn DFA.ANY => true | _ => false) l)
		  then ALL
		  else PARTIAL
	(* compute the set of elements in the list of patterns and return
	 * the coverage of the set.
	 *)
	  fun chkSet (cvtFn, coverFn) l = let
		fun add (s, item) = S.add(s, cvtFn item)
		fun chk (s, []) = if (coverFn s) then ALL else PARTIAL
		  | chk (s, DFA.ANY :: r) = ALL
		  | chk (s, DFA.TPL _ :: r) = ALL
		  | chk (s, pat :: r) = chk(add(s, pat), r)
		in
		  chk (S.empty, l)
		end
	(* cvtFn and coverFn for datatype patterns *)
	  fun cvtDataty (DFA.CON(dc, _, _)) = DC.idOf dc
	    | cvtDataty pat = raise Fail "coverage.cvtDataty: bogus pattern"
	  fun coverDataty (Ty.Tyc{def=Ty.DataTyc{nCons, ...}, ...}) s = (S.numItems s = !nCons)
	(* check the coverage of a list of patterns *)
	  fun chk [] = PARTIAL
	    | chk (DFA.ANY :: r) = ALL
	    | chk (DFA.LIT _ :: r) = chkForAny r
	    | chk (DFA.TPL _ :: r) = ALL
	    | chk (l as (DFA.CON(dc, _, _) :: r)) = 
		chkSet (cvtDataty, coverDataty(DC.ownerOf dc)) l
	  in
	    chk pats
	  end
    end (* local *)

  (* Information in a constructor map *)
    type cons_info = {
	pat : DFA.simple_pat,	(* but not ANY! *)
	mat : matrix ref
      }

    type con_map = cons_info list

  (* split a pattern matrix based on the constructors of the given column.
   * For each constructor in the selected column, we construct a new pattern
   * matrix that contains a row for each row that matches the constructor.
   * This new matrix includes any rows where there is a variable in the selected
   * column.
   * Note that it is important that the order of constructors be preserved
   * and that the order of rows that have the same constructor also be preserved.
   *)
    fun splitAtCol (M{rows, cols, vars}, i) = let
	(* find the entry for a constructor in the conMap *)
	  fun findLit (conMap : con_map, l : L.literal) = let
		fun find [] = NONE
		  | find ({pat=DFA.LIT lit, mat}::r) =
		      if L.same(lit, l) then SOME mat else find r
		  | find (_::r) = find r
		in
		  find conMap
		end
	(* get the entry for a tupe in the conMap.  Note that the conMap can have 0 or 1
	 * entries in this case!
	 *)
	  fun findTpl [] = NONE
	    | findTpl [{pat=DFA.TPL _, mat}] = SOME mat
	    | findTpl _ = raise Fail "bogus conMap for tuple split"
	(* find the entry for a constructor in the conMap *)
	  fun findCon (conMap : con_map, c) = let
		fun find [] = NONE
		  | find ({pat=DFA.CON(c', _, _), mat}::r) =
		      if DC.same(c, c') then SOME mat else find r
		  | find (_::r) = find r
		in
		  find conMap
		end
	(* create the initial conMap (one entry per constructor in the
	 * column).
	 *)
	  fun mkConMap NIL = []
	    | mkConMap (CELL{down, pat, ...}) = let
		val conMap = mkConMap down
		in
		  case pat
		   of P_Wild => conMap
		    | (P_Lit(lit, ty)) => (case findLit(conMap, lit)
			 of NONE => let
			      val vars = expandVars(vars, i, [])
			      val mat = mkNilMat vars
			      val conMap = {pat=DFA.LIT lit, mat = ref mat} :: conMap
			      in
				conMap
			      end
			  | (SOME _) => conMap
			(* end case *))
		    | (P_Con(c, tys)) => (case findCon(conMap, c)
			 of NONE => let
			      val vars = expandVars(vars, i, [])
			      val mat = mkNilMat vars
			      val conMap = {pat=DFA.CON(c, tys, []), mat = ref mat} :: conMap
			      in
				conMap
			      end
			  | (SOME _) => conMap
			(* end case *))
		    | (P_ConApp(c, tys, path, pat)) => (
			case findCon(conMap, c)
			 of NONE => let
			      val vars = expandVars(vars, i, [path])
			      val mat = mkNilMat vars
			      val conMap = {
				      pat=DFA.CON(c, tys, [path]), mat = ref mat
				    } :: conMap
			      in
				conMap
			      end
			  | (SOME _) => conMap
			(* end case *))
		    | P_Tuple pats => (
			case findTpl conMap
			 of NONE => let
			      val paths = List.map #1 pats
			      val vars = expandVars(vars, i, paths)
			      val mat = mkNilMat vars
			      in
				[{pat=DFA.TPL paths, mat = ref mat}]
			      end
			  | (SOME _) => conMap
			(* end case *))
		  (* end case *)
		end
	  val splitCol = List.nth(cols, i)
	  val conMap = mkConMap splitCol
	(* populate the conMap and build the varMat *)
	  fun f ([], _) = mkNilMat vars
	    | f (row::rows, CELL{pat, right, down}) = let
		  val varMat = f (rows, down)
		  in
		    case pat
		     of P_Wild => let
			  fun addVarRow {pat, mat} =
				mat := addRow(!mat,
				  expandCols(row, i,  map (fn v => (v, P_Wild)) (DFA.pathsOf pat)))
			  in
			  (* we add the row to all of the sub-matrices *)
			    List.app addVarRow conMap;
			    addRow(varMat, row)
			  end
		      | P_Lit(lit, _) => let
			  val (SOME mat) = findLit (conMap, lit)
			  in
			    mat := addRow(!mat, expandCols(row, i, []));
			    varMat
			  end
		      | P_Con(c, _) => let
			  val (SOME mat) = findCon (conMap, c)
			  in
			    mat := addRow(!mat, expandCols(row, i, []));
			    varMat
			  end
		      | P_ConApp(c, _, path, pat) => let
			  val (SOME mat) = findCon (conMap, c)
			  in
			    mat := addRow(!mat, expandCols(row, i, [(path, pat)]));
			    varMat
			  end
		      | P_Tuple pats => let
			  val [{mat, ...}] = conMap
			  in
			    mat := addRow(!mat, expandCols(row, i, pats));
			    varMat
			  end
		    (* end case *)
		  end
	  val varMat = f (rows, splitCol)
	  val coverage = coverage(map #pat conMap)
	  in
	    (List.nth(vars, i), conMap, varMat, coverage)
	  end

  (* sets of constructors and/or literals *)
    datatype con_or_lit = CC of AST.dcon | LL of Literal.literal

    structure ConSet = RedBlackSetFn (
      struct
	type ord_key = con_or_lit
	fun compare (CC c1, CC c2) = DC.compare(c1, c2)
	  | compare (CC _, LL _) = LESS
	  | compare (LL _, CC _) = GREATER
	  | compare (LL l1, LL l2) = Literal.compare(l1, l2)
      end)

  (* choose a column of a matrix for splitting; currently we choose the column
   * with a constructor in its first row and the largest number of distinct
   * constructors.  If none of the columns have constants or constructors, we
   * return one that has a leading tuple pattern; otherwise we return NONE.
   *)
    fun chooseCol (M{rows, cols, vars}) = let
	(* count the number of constants/constructors in the column *)
	  fun count (NIL, cons) = ConSet.numItems cons
	    | count (CELL{pat, down, ...}, cons) = let
		val cons = (case pat
		       of P_Wild => cons
			| P_Con(c, _) => ConSet.add(cons, CC c)
			| P_ConApp(c, _, _, _) => ConSet.add(cons, CC c)
			| P_Lit(lit, _) => ConSet.add(cons, LL lit)
			| P_Tuple _ => cons
		      (* end case *))
		in
		  count (down, cons)
		end
	(* find the row with the largest number of distinct constants/constructors *)
	  fun maxRow (curMax, curCnt, _, []) = curMax
	    | maxRow (curMax, curCnt, i, CELL{pat=P_Wild, ...}::cols) =
		maxRow (curMax, curCnt, i+1, cols)
	    | maxRow (curMax, curCnt, i, col::cols) = let
		val cnt = count(col, ConSet.empty)
		in
		  if (cnt > curCnt)
		    then maxRow (SOME i, cnt, i+1, cols)
		    else maxRow (curMax, curCnt, i+1, cols)
		end
	  in
	    case maxRow (NONE, 0, 0, cols)
	     of NONE => let (* check for a tuple that we can expand *)
		  fun findTpl (i, CELL{pat = P_Tuple _, ...} :: _) = SOME i
		    | findTpl (i, _::r) = findTpl (i+1, r)
		    | findTpl _ = NONE
		  in
		    findTpl (0, cols)
		  end
	      | someCol => someCol
	    (* end case *)
	  end


  (******************** Translation ********************)

    type rule_info = {		(* A TypedAST match rule with additional info *)
	loc : Error.span,	  (* the source location of the rule *)
	pat : AST.pat,	  	  (* the lhs pattern *)
	optCond : AST.exp option, (* optional "where" clause *)
	bvs : VSet.set,		  (* source variables bound in pats *)
	act : AST.exp		  (* the rhs action *)
      }

  (* the first step converts a list of rule info to a pattern matrix.  We do
   * this by first doing a renaming pass that also  creates the initial states.
   * Then we invoke mkMatrix to build the initial pattern matrix.  We return the
   * pattern matrix and the error state.
   *)
    fun step1 (dfa, rules : rule_info list) = let
	(* Convert a AST pattern to a simplified pattern.  We take as arguments
	 * the source-file location, the pattern's path, the pattern's type,
	 * and the pattern.
	 *)
	  fun doPat (loc, path, AST.ConPat(dc, tys, pat)) = let
		val SOME argTy = DataCon.argTypeOf'(dc, tys)
		val argPath = DFA.extendPath(path, 0, argTy)
		val (pat', vm) = doPat(loc, argPath, pat)
		in
		  (P_ConApp(dc, tys, argPath, pat'), vm)
		end
	    | doPat (loc, path, AST.TuplePat pats) = let
		val (pats', vm) = doPatList(loc, path, pats)
		in
		  (P_Tuple pats', vm)
		end
	    | doPat (loc, path, AST.VarPat x) = (P_Wild, VMap.singleton(x, path))
	    | doPat (loc, path, AST.WildPat _) = (P_Wild, VMap.empty)
	    | doPat (loc, path, AST.ConstPat(AST.DConst(con, tys))) =
		(P_Con(con, tys), VMap.empty)
	    | doPat (loc, path, AST.ConstPat(AST.LConst(lit, ty))) =
		(P_Lit(lit, ty), VMap.empty)
	(* convert a list of patterns (i.e., a tuple of patterns) to a list
	 * of simplified patterns.
	 *)
	  and doPatList (loc, parentPath, pats) = let
	      (* expand each of the subpatterns *)
		fun doSubPats (_, [], []) = ([], VMap.empty)
		  | doSubPats (i, ty::tys, p::ps) = let
		      val (ps', vm') = doSubPats (i+1, tys, ps)
		      val path = DFA.extendPath(parentPath, i, ty)
		      val (p', vm) = doPat (loc, path, p)
		      in
			((path, p') :: ps', union(vm, vm'))
		      end
		in
		  doSubPats (0, List.map TypeOf.pat pats, pats)
		end
	(* Compute the initial list of paths *)
	  val (topPath, argTy) = let
		val arg = DFA.getArg dfa
		in
		  (DFA.ROOT arg, Var.typeOf arg)
		end
	  fun doRule {loc, pat, optCond, bvs, act} = let
		val (simplePat, vm) = doPat (loc, topPath, pat)
		val q = DFA.mkFinal(dfa, bvs, act)
		val optCond = Option.map (fn e => (bvs, e)) optCond
		in
		  (simplePat, optCond, vm, q)
		end
	  val matrix = mkMatrix (topPath, List.map doRule rules)
	  in
	    matrix
	  end

  (* The second step translates the pattern matrix into the DFA representation.
   * This translation is done bottom-up.
   *)
    fun step2 (patMatrix : matrix, dfa) = let
	  val errState = DFA.errorState dfa
	  fun genDFA (mat as M{rows as row1::rrows, cols, vars}) = let
		val R{vmap, cells, optCond, act} = row1
		in
(* +DEBUG **
print(concat["genDFA: ", Int.toString(length rows), " rows, ",
Int.toString(length cols), " cols\n"]);
** -DEBUG *)
		  case (optCond, chooseCol mat)
		   of (NONE, NONE) => DFA.mkBind(dfa, vmap, act)
		    | (SOME(bvs, e), NONE) => (case rrows
			 of [] => DFA.mkCond(dfa, vmap, e, act, errState)
			  | ((row as R{cells, ...})::_) =>
			      DFA.mkCond(dfa, vmap, e,
				act,
				genDFA(M{
				    rows = rrows, cols = rowToList cells,
				    vars = vars
				  }))
			(* end case *))
		    | (_, SOME i) => let
(* FIXME: splitAtCol should return a revised vmap that has the variables
 * which are bound ???
 *)
			val (splitVar, conMap, varMat, coverage) =
			      splitAtCol(mat, i)
(* +DEBUG **
val _ = print(concat["  split at column ", Int.toString i,
"; coverage is ", coverToString coverage, "\n"]);
** -DEBUG *)
			val lastArc = (case (varMat, coverage)
			       of (_, ALL) => []
				| (M{rows=[], ...}, _) => let
				    fun mkCell (_, (right, cols)) = let
					  val cell = CELL{
						  pat = P_Wild, down = NIL,
						  right = right
						}
					  in
					    (cell, cell::cols)
					  end
				    val (row, cols) =
					  List.foldr mkCell (NIL, []) vars
				    val r = R{
					    vmap = Var.Map.empty,
					    cells = row,
					    optCond = NONE,
					    act = errState
					  }
				    val mat = M{
					    rows=[r], cols=cols, vars=vars
					  }
				    in
				      [(DFA.ANY, genDFA mat)]
				    end
			 	| (mat, _) => [(DFA.ANY, genDFA mat)]
			      (* end case *))
			fun mkArc ({pat, mat}, arcs) =
			      (pat, genDFA(!mat)) :: arcs
			val arcs = List.foldr mkArc lastArc conMap
			in
			  DFA.mkTest(dfa, splitVar, arcs)
			end
		  (* end case *)
		end
	  val root = genDFA patMatrix
	  in
	    DFA.setInitialState (dfa, root)
	  end

    fun rulesToDFA (loc, arg, rules) = let
	  fun ruleInfo (loc, pat, optCond, act) = {
		  loc = loc, pat = pat, optCond = optCond,
		  bvs = MatchUtil.varsOfPat pat, act = act
		}
	  fun cvtRule loc (AST.PatMatch(pat, act)) = ruleInfo (loc, pat, NONE, act)
	    | cvtRule loc (AST.CondMatch(pat, cond, act)) = ruleInfo (loc, pat, SOME cond, act)
	  val dfa = DFA.mkDFA arg
	  val rules = List.map (cvtRule loc) rules
	  val patMatrix = step1 (dfa, rules)
	  in
	    step2 (patMatrix, dfa);
	    dfa
	  end

  end
