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

    val rulesToDFA : (Error.location * MatchCompEnv.env * AST.var * (AST.pat * AST.exp) list)
	  -> MatchDFA.dfa

  end = struct

    structure L = Literal
    structure Ty = Types
    structure DFA = MatchDFA
    structure Env = MatchCompEnv
    structure DC = DataCon
    structure VMap = Var.Map
    structure VSet = Var.Set

(*FIXME*)val dummy : DFA.var_map = VMap.empty


  (******************** Utility code ********************)

  (* given a list of AST patterns, compute the set of variables
   * bound by the pattern.
   *)
    fun analysePats pats = let
	  fun analyse (AST.ConPat(_, _, ps)) = analyseList(pats, vs)
	    | analyse (AST.TuplePat ps, vs) = analyseList(pats, vs)
	    | analyse (AST.VarPat x, vs) = VSet.add(vs, x)
	    | analyse (AST.ConstPat _, vs) = vs
	  and analyseList ([], vs) = vs
	    | analyseList (pat::pats, vs) = analyseList(pats, analyse(pat, vs))
	  in
	    analyseList (pats, VSet.empty)
	  end


  (******************** Pattern matrix ********************)

  (* simplified source patterns (after variable renaming) *)
    datatype pat
      = P_Wild
      | P_Lit of (Literal.literal * Ty.ty)
      | P_Con of AST.dcon * Ty.ty list
      | P_ConApp of (AST.dcon * Ty.ty list * pats)

  (* a tuple of patterns, with a mapping from variables bound to the
   * element positions to their paths.
   *)
    and pats = PATS of (DFA.var_map * (DFA.path * pat) list)

    datatype cell
      = NIL
      | CELL of {
	  pat : pat,
	  right : cell,
	  down : cell
	}

  (* a row of cells in a pattern matrix *)
    datatype row = R of {
	  vmap : DFA.var_map,		(* variables bound to cells in this *)
					(* row, with their mapping to paths. *)
	  cells : cell,			(* cell of the first column *)
	  optWhen : (VSet.set * AST.exp) option, (* optional when clause *)
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

  (* create a pattern matrix from a list of rows. *)
    fun mkMatrix (match as ((PATS(_, row1), _, _)::_)) = let
	  val vars = map #1 row1
	  fun mkRows [] = (List.map (fn _ => NIL) vars, [])
	    | mkRows ((PATS(vmap, row), optWhen, q)::rows) = let
		fun doCols ([], []) = NIL
		  | doCols ((_, pat)::r, cell::right) = CELL{
			pat = pat, right = doCols (r, right), down = cell
		      }
		val (topRow, rows) = mkRows rows
		val newRow = doCols (row, topRow)
		val row = R{vmap=vmap, cells=newRow, optWhen=optWhen, act=q}
		in
		  (rowToList newRow, row::rows)
		end
	  val (topRow, rows) = mkRows match
	  in
	    M{ rows = rows, cols = topRow, vars = vars }
	  end

  (* add a row to the top of a matrix *)
    fun addRow (M{rows, cols, vars}, R{vmap, cells, optWhen, act}) = let
	  fun cons (NIL, []) = (NIL, [])
	    | cons (CELL{pat, right = r1, ...}, dn::r2) = let
		val (right, cols) = cons(r1, r2)
		val cell = CELL{pat = pat, right = right, down = dn}
		in
		  (cell, cell::cols)
		end
	  val (row, cols) = cons (cells, cols)
	  val r = R{vmap=vmap, cells=row, optWhen=optWhen, act=act}
	  in
	    M{rows = r :: rows, cols = cols, vars = vars}
	  end

  (* replace the ith variable with newVars *)
    fun expandVars (vars, i, newVars) = let
	  fun ins (0, _::r) = newVars @ r
	    | ins (i, v::r) = v :: ins(i-1, r)
	  in
	    ins (i, vars)
	  end

  (* replace the ith cell of a row with the expansion of args *)
    fun expandCols (R{vmap, cells, optWhen, act}, i, vm, args) = let
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
	  val vmap = VMap.unionWith (fn _ => raise Fail "overlap") (vmap, vm)
	  in
(* FIXME: we need to remove the variables bound in the ith column from vmap! *)
	    R{vmap = vmap, cells = ins (i, cells), optWhen = optWhen, act = act}
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
		  | chk (s, DFA.ANY::r) = ALL
		  | chk (s, DFA.CON(AST.AbsCon _, _, _)::r) = chk(s, r)
		  | chk (s, pat::r) = chk(add(s, pat), r)
		in
		  chk (S.empty, l)
		end
	(* cvtFn and coverFn for Bool patterns *)
	  fun cvtBool (DFA.LIT(L.Bool false)) = 0
	    | cvtBool (DFA.LIT(L.Bool true)) = 1
	    | cvtBool pat = raise Fail "coverage.cvtBool: bogus pattern"
	  fun coverBool s = (S.numItems s = 2)
	(* cvtFn and coverFn for Char patterns *)
	  fun cvtChar (DFA.LIT(L.Char c)) = Word.toIntX c
	    | cvtChar pat = raise Fail "coverage.cvtChar: bogus pattern"
	  fun coverChar s = (S.numItems s = 65536)
	(* cvtFn and coverFn for enum patterns *)
	  fun cvtEnum (DFA.CON(AST.EnumCon(_, tag), _, _)) = tag
	    | cvtEnum pat = raise Fail "coverage.cvtEnum: bogus pattern"
	  fun coverEnum (Ty.Tyc{kind=Ty.ENUM{nCons, ...}, ...}) s = (S.numItems s = nCons)
	(* cvtFn and coverFn for datatype patterns *)
	  fun cvtDataty (DFA.CON(AST.DTCon(_, tag), _, _)) = tag
	    | cvtDataty pat = raise Fail "coverage.cvtDataty: bogus pattern"
	  fun coverDataty (Ty.Tyc{kind=Ty.DATA{cons, ...}, ...}) s =
		(S.numItems s = List.length(!cons))
	(* check for either root tagtype constructors or DFA.ANY *)
	  fun chkTagtyCover [] = PARTIAL
	    | chkTagtyCover (DFA.ANY::r) = ALL
	    | chkTagtyCover (
		DFA.CON(AST.TagCon(Ty.Tyc{kind=Ty.TAG{parent=ref NONE, ...}, ...}), _, _)::_
	      ) = ALL
	    | chkTagtyCover (_::r) = chkTagtyCover r
	(* check the coverage of a list of patterns *)
	  fun chk [] = PARTIAL
	    | chk (DFA.ANY::r) = ALL
	    | chk (l as (DFA.LIT(lit)::r)) = (case lit
		 of (L.Bool _) => chkSet (cvtBool, coverBool) l
		  | (L.Char c) => chkSet (cvtChar, coverChar) l
		  | _ => chkForAny r
		(* end case *))
	    | chk (l as (DFA.CON(dc, _, _)::r)) = (case dc
		 of (AST.EnumCon(ety, tag)) => chkSet (cvtEnum, coverEnum ety) l
		  | (AST.DTCon(dty, tag)) => chkSet (cvtDataty, coverDataty dty) l
		  | (AST.TagCon _) => chkTagtyCover l
		  | (AST.ConstCon _) => raise Fail "unexpected ConstCon"
		  | (AST.AbsCon _) => chk r
		  | (AST.TupleCon _) => ALL
		(* end case *))
	  in
	    chk pats
	  end
    end (* local *)

  (* Constructor map *)
    type cons_info = {
	pat : DFA.simple_pat,	(* but not ANY! *)
	mat : matrix ref
      }

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
	  fun findCon (conMap : cons_info list, c) = let
		fun find [] = NONE
		  | find ({pat=DFA.CON(c', _, _), mat}::r) =
		      if DC.same(c, c') then SOME mat else find r
		  | find (_::r) = find r
		in
		  find conMap
		end
	(* find the entry for a constructor in the conMap *)
	  fun findLit (conMap : cons_info list, l) = let
		fun find [] = NONE
		  | find ({pat=DFA.LIT lit, mat}::r) =
		      if Literal.same(lit, l) then SOME mat else find r
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
			      val conMap =
				    {pat=DFA.LIT lit, mat = ref mat} :: conMap
			      in
				conMap
			      end
			  | (SOME _) => conMap
			(* end case *))
		    | (P_Con(c, tys)) => (case findCon(conMap, c)
			 of NONE => let
			      val vars = expandVars(vars, i, [])
			      val mat = mkNilMat vars
			      val conMap =
				    {pat=DFA.CON(c, tys, []), mat = ref mat} :: conMap
			      in
				conMap
			      end
			  | (SOME _) => conMap
			(* end case *))
		    | (P_ConApp(c, tys, PATS(_, args))) => (
			case findCon(conMap, c)
			 of NONE => let
			      val argVars = map #1 args
			      val vars = expandVars(vars, i, argVars)
			      val mat = mkNilMat vars
			      val conMap = {
				      pat=DFA.CON(c, tys, argVars), mat = ref mat
				    } :: conMap
			      in
				conMap
			      end
			  | (SOME _) => conMap
			(* end case *))
		  (* end case *)
		end
	  val splitCol = List.nth(cols, i)
	  val conMap = mkConMap splitCol
	(* populate the conMap and build the varMap *)
	  fun f ([], _) = mkNilMat vars
	    | f (row::rows, CELL{pat, right, down}) = let
		  val varMat = f (rows, down)
		  in
		    case pat
		     of P_Wild => let
			  fun addVarRow {pat, mat} =
				mat := addRow(!mat,
				  expandCols(row, i, VMap.empty,
				    map (fn v => (v, P_Wild)) (DFA.pathsOf pat)))
			  in
			  (* we add the row to all of the sub-matrices *)
			    List.app addVarRow conMap;
			    addRow(varMat, row)
			  end
		      | (P_Lit(lit, _)) => let
			  val (SOME mat) = findLit (conMap, lit)
			  in
			    mat := addRow(!mat, expandCols(row, i, VMap.empty, []));
			    varMat
			  end
		      | (P_Con(c, _)) => let
			  val (SOME mat) = findCon (conMap, c)
			  in
			    mat := addRow(!mat, expandCols(row, i, VMap.empty, []));
			    varMat
			  end
		      | (P_ConApp(c, _, PATS(vmap, args))) => let
			  val (SOME mat) = findCon (conMap, c)
			  in
			    mat := addRow(!mat, expandCols(row, i, vmap, args));
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
   * constructors.  If all the columns start with a variable, return NONE.
   *)
    fun chooseCol (M{rows, cols, vars}) = let
	  fun count (NIL, cons) = ConSet.numItems cons
	    | count (CELL{pat, down, ...}, cons) = let
		val cons = (case pat
		       of P_Wild => cons
			| ((P_Con(c, _)) | (P_ConApp(c, _, _))) =>
			    ConSet.add(cons, CC c)
			| P_Lit(lit, _) => ConSet.add(cons, LL lit)
		      (* end case *))
		in
		  count (down, cons)
		end
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
	    maxRow (NONE, 0, 0, cols)
	  end


  (******************** Translation ********************)

    type rule_info = {		(* A TypedAST match rule with additional info *)
	loc : Error.location,	  (* the source location of the rule *)
	pats : AST.pat list,	  (* the lhs patterns *)
	isDefault : bool,	  (* true, if this is the synthesized default rule *)
	optWhen : AST.exp option, (* optional "when" clause *)
	bvs : VSet.set,		  (* source variables bound in pats *)
	act : AST.exp		  (* the rhs action *)
      }

  (* the first step converts a list of rule info to a pattern
   * matrix.  We do this by first doing a renaming pass that also expands
   * the or-patterns and creates the initial states.  Then we invoke mkMatrix
   * to build the pattern matrix.  We return the pattern matrix and the error
   * state.
   *)
    fun step1 (env, dfa, rules : rule_info list) = let
	(* Convert a pattern to a list of simplified patterns by expanding
	 * or patterns.  We take as arguments the source-file location,
	 * the pattern's path, the pattern's type, and the pattern.
	 *)
	  fun doPat (loc, path, ty, AST.P_VAR x) =
		[(path, P_Wild)]
	    | doPat (loc, path, ty, AST.P_IS(x, pat)) =
		doPat (loc, path, ty, pat)
	    | doPat (loc, path, ty, AST.P_OR(p1, p2)) = let
		val ps1 = doPat(loc, path, ty, p1)
		val ps2 = doPat(loc, path, ty, p2)
		in
		  ps1 @ ps2
		end
	    | doPat (loc, path, ty, pat) = let
		fun doCon (loc, con, tys) = (case expandCon(env, con, [])
		       of NONE => [(path, P_Con(con, tys))]
			| SOME pat => doPat(loc, path, ty, pat)
		      (* end case *))
		fun doConApp (loc, con, tys, argPats) = (
		      case expandCon(env, con, argPats)
		       of NONE => let
			    fun cons row = (path, P_ConApp(con, tys, row))
			    val tys = List.map TypeOf.pat argPats
			    in
			      List.map cons
				(doPatList(loc, path, tys, argPats))
			    end
			| SOME pat => doPat(loc, path, ty, pat)
		      (* end case *))
		in
		  case pat
		   of AST.P_WILD _ => [(path, P_Wild)]
		    | AST.P_LIT(lit, ty) => [(path, P_Lit(lit, ty))]
		    | AST.P_CON(con, loc) => doCon (loc, con, [])
		    | AST.P_CONAPP(con, pats, loc) => doConApp (loc, con, [], pats)
		    | AST.P_TCON(con, tys, loc) => doCon (loc, con, tys)
		    | AST.P_TCONAPP(con, tys, pats, loc) => doConApp (loc, con, tys, pats)
		    | AST.P_ISNOT _ => raise Fail "P_ISNOT"
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	(* expand a list of patterns (e.g., tuple of patterns).  This
	 * function returns a list of pats, where the
	 * number of patterns in each of the simplified-pattern lists is the
	 * same as the number of patterns in the input list.
	 *)
	  and doPathPatList (loc, paths, tys, pats) = let
	      (* compute the var to path mapping for pats *)
		val vmap = let
		      fun f (pat, path, vm) =
			    List.foldl
			      (fn (x, vm) => VMap.insert(vm, x, path))
				vm
				  (bvsOfPat pat)
		      in
			ListPair.foldl f VMap.empty (pats, paths)
		      end
	      (* expand each of the subpatterns *)
		fun doSubPats (_, [], [], []) = []
		  | doSubPats (i, path::paths, ty::tys, p::ps) =
		      doPat (loc, path, ty, p)
			:: doSubPats (i+1, paths, tys, ps)
	      (* merge the expanded subpatterns by taking their cross
	       * product.  Note: this is where we can get exponential
	       * blowup in the pattern representation!
	       *)
		fun merge [] = [[]] : (DFA.path * pat) list list
		  | merge ((ps : (DFA.path * pat) list) :: pss) = let
		      val pss = merge pss
		      in
			List.foldr
			  (fn (x, acc) => (List.map (fn l => x::l) pss) @ acc)
			    [] ps
		      end
		val pss = merge (doSubPats (0, paths, tys, pats))
		in
		  List.map (fn ps => PATS(vmap, ps)) pss
		end
	  and doPatList (loc, parentPath, tys, pats) = let
		fun extPath (_, []) = []
		  | extPath (i, ty::tys) =
		      DFA.extendPath(parentPath, i, ty) :: extPath (i+1, tys)
		in
		  doPathPatList (loc, extPath(0, tys), tys, pats)
		end
	(* Compute the initial list of paths *)
	  val (topPaths, argTys) = let
		fun f (x, (xs, tys)) = (DFA.ROOT x :: xs, Var.typeOf x :: tys)
		in
		  List.foldr f ([], []) (DFA.getArgs dfa)
		end
	  fun doRule {loc, pats, optWhen, isDefault, bvs, act} = let
		val q = DFA.mkFinal(dfa, isDefault, bvs, act)
		val optWhen = Option.map (fn e => (bvs, e)) optWhen
		fun mkRow row = (row, optWhen, q)
		in
		  List.map mkRow
		    (doPathPatList (loc, topPaths, argTys, pats))
		end
	  fun doRules [] = []
	    | doRules (rule::rest) = doRule rule @ doRules rest
(* NOTE: eventually, we can merge mkMatrix into this function. *)
	  val matrix = mkMatrix (doRules rules)
	  in
	    matrix
	  end

  (* The second step translates the pattern matrix into the DFA representation.
   * This translation is done bottom-up.
   *)
    fun step2 (patMatrix : matrix, dfa) = let
	  val errState = DFA.errorState dfa
	  fun genDFA (mat as M{rows as row1::rrows, cols, vars}) = let
		val R{vmap, cells, optWhen, act} = row1
		in
(*DEBUG print(concat["genDFA: ", Int.toString(length rows), " rows, ", *)
(*DEBUG   Int.toString(length cols), " cols\n"]); *)
		  case (optWhen, chooseCol mat)
		   of (NONE, NONE) => DFA.mkBind(dfa, vmap, act)
		    | (SOME(bvs, e), NONE) => (case rrows
			 of [] => DFA.mkWhen(dfa, vmap, e, act, errState)
			  | ((row as R{cells, ...})::_) =>
			      DFA.mkWhen(dfa, vmap, e,
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
(*DEBUG val _ = print(concat["  split at column ", Int.toString i, *)
(*DEBUG "; coverage is ", coverToString coverage, "\n"]); *)
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
(*FIXME*)vmap = Var.Map.empty,
					    cells = row,
					    optWhen = NONE,
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

    fun rulesToDFA (loc, env, args, rules) = let
	  fun ruleInfo (loc, pats, optWhen, isDefault, act) = {
		  loc = loc, pats = pats, optWhen = optWhen,
		  isDefault = isDefault,
		  bvs = analysePats pats, act = act
		}
	  fun cvtRule loc (AST.M_MATCH(pats, act)) =
		ruleInfo (loc, pats, NONE, false, act)
	    | cvtRule loc (AST.M_MATCHWHEN(pats, exp, act)) =
		ruleInfo (loc, pats, SOME exp, false, act)
	    | cvtRule loc (AST.M_DEFAULT(pats, act)) =
		ruleInfo (loc, pats, NONE, true, act)
	  val dfa = DFA.mkDFA args
	  val rules = List.map (cvtRule loc) rules
	  val patMatrix = step1 (env, dfa, rules)
	  in
	    step2 (patMatrix, dfa);
	    dfa
	  end

  end
