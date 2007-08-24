(* match-compile.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Translate bindings and matches to simple matches.
 *)

structure MatchCompile : sig

  (* expand bindings and pattern matches to simplified form *)
    val compile : AST.module -> AST.module

  end = struct

    structure L = Literal
    structure Ty = Types
    structure TU = TypeUtil
    structure DFA = MatchDFA
    structure VSet = Var.Set
    structure VMap = Var.Map

  (* hash tables on DFA states *)
    structure STbl = HashTableFn (
      struct
        type hash_key = DFA.state
	val hashVal = DFA.hash
	val sameKey = DFA.same
      end)

  (* finite sets and maps with DFA path keys *)
    local
      structure POrd =
	struct
	  type ord_key = DFA.path
	  val compare = DFA.comparePath
	end
    in
    structure PSet = RedBlackSetFn (POrd)
    structure PMap = RedBlackMapFn (POrd)
    end (* local *)

    fun newVar ty = Var.new("_anon_", ty)

  (* compute the set of free source variables and used path variables
   * for the states of a DFA.  Record this information for shared states
   * and for the initial state.  Return the list of shared states and
   * the getInfo function.
   *)
    fun analyseDFA dfa = let
	  val stateInfo = STbl.mkTable(DFA.size dfa, Fail "state info")
	  val findInfo = STbl.find stateInfo
	  val setInfo = STbl.insert stateInfo
	(* list of shared states *)
	  val shared = ref[]
	(* return vset\dom(vmap) *)
	  fun subtractDom (vset, vmap) = VSet.difference(
		vset,
		VSet.addList(VSet.empty, VMap.listKeys vmap))
	(* walk the DFA gathering information about the states *)
	  fun next q = (case findInfo q
		 of NONE => let
		      val info = visit q
		      in
			if (DFA.rCount q > 1)
			  then shared := q :: !shared
			  else ();
			setInfo (q, info);
			info
		      end
		  | SOME info => info
		(* end case *))
	  and visit q = (case DFA.kind q
		 of DFA.TEST(arg, cases) => let
		      fun doCase ((pat, arc), {pvs, fvs}) = let
			    val {pvs=pvs', fvs=fvs'} = next arc
			    in {
			      pvs = PSet.union(pvs,
				PSet.difference(
				  pvs',
				  PSet.addList(PSet.empty, DFA.pathsOf pat))),
			      fvs = VSet.union(fvs, fvs')
			    } end
		      in
			List.foldl doCase {pvs=PSet.singleton arg, fvs=VSet.empty} cases
		      end
		  | DFA.BIND(vmap, q) => let
		      val {pvs, fvs} = next q
		      in {
			pvs = VMap.foldl PSet.add' pvs vmap,
			fvs = subtractDom(fvs, vmap)
		      } end
		  | DFA.FINAL(_, fvs, _) => {pvs = PSet.empty, fvs = fvs}
		  | DFA.WHEN(vmap, _, arc1, arc2) => let
		      val {pvs=pvs1, fvs=fvs1} = next arc1
		      val {pvs=pvs2, fvs=fvs2} = next arc2
		      in {
			pvs = PSet.union(pvs1, pvs2),
			fvs = subtractDom (VSet.union(fvs1, fvs2), vmap)
		      } end
		  | DFA.ERROR => {pvs = PSet.empty, fvs = VSet.empty}
		(* end case *))
	  val initialState = DFA.initialState dfa
	  in
	    ignore (next initialState);
	    {shared = !shared, infoOf = STbl.lookup stateInfo}
	  end


    type env = AST.var VMap.map

    val insertVar : (env * AST.var * AST.var) -> env = VMap.insert

    fun applyEnv (env, x) = (case VMap.find(env, x)
	   of SOME x' => x'
	    | NONE => x
	  (* end case *))

    datatype decision_tree
      = CALL of AST.var * AST.var list
      | CASE of (AST.var * (simple_pat * decision_tree) list)
      | IF of (env * AST.exp * decision_tree * decision_tree)
      | ACTION of (env * AST.exp)

    and simple_pat
      = WILD
      | VAR of AST.var
      | LIT of L.literal
      | CON of (AST.data_con * Ty.ty list * pat_arg list)

    and pat_arg
      = ARG_ANON of Ty.ty
      | ARG_VAR of AST.var

  (* given an initial environment, DFA, and error action, build a forest of
   * decision trees representing the DFA.
   *)
    fun dfaToTrees (loc, env0 : env, dfa, raiseExn, resTy) = let
	  val {shared, infoOf} = analyseDFA dfa
	  val root = DFA.initialState dfa
	  val stateFuns = STbl.mkTable (8, Fail "shared state funs")
	  fun lookupPath (pmap, path) = (case PMap.find(pmap, path)
		 of SOME x => x
		  | NONE => raise Fail(concat["lookupPath(", DFA.pathToString path, ")"])
		(* end case *))
	(* extend the renaming environment env by the composition of
	 * vmap, which maps variables to paths, with pmap, which maps
	 * paths to variables.
	 *)
	  fun extendEnvByVMap (env, pmap, vmap) = let
		fun ext (x, path, env) = (case PMap.find(pmap, path)
		       of SOME x' =>
			    if not(Var.same(x, x'))
			      then insertVar(env, x, x')
			      else env
			| NONE => raise Fail(concat[
			      "ext(", Var.fmt {full=true} x, ",",
			      DFA.pathToString path, ")"
			    ])
		      (* end case *))
		in
		  VMap.foldli ext env vmap
		end
	  fun next (env, pmap, q) = if (DFA.rCount q > 1)
		then let
		  val (f, _, _) = STbl.lookup stateFuns q
		  val {pvs, fvs} = infoOf q
		  val fvars = VSet.foldr
			(fn (x, ys) => applyEnv(env, x) :: ys)
			  [] fvs
		  val pvars = PSet.foldr
			(fn (path, ys) => lookupPath(pmap, path)::ys)
			  [] pvs
		  in
		    CALL(f, fvars @ pvars)
		  end
		else visit(env, pmap, q)
	(* convert the state q to a decision tree; the env is the renaming
	 * environment to be used at this point and the pmap is a mapping
	 * from used path variables to the typed AST variables that are
	 * bound to them.
	 *)
	  and visit (env, pmap, q) = (case DFA.kind q
		 of DFA.TEST(arg, cases) => let
		      fun nextArc arc = next(env, pmap, arc)
		      fun cvtCase (DFA.ANY, arc) = (case PMap.find(pmap, arg)
			     of NONE => (WILD, nextArc arc)
			      | SOME x => (VAR x, nextArc arc)
			    (* end case *))
			| cvtCase (DFA.LIT lit, arc) = (LIT lit, nextArc arc)
			| cvtCase (DFA.CON(dc, tys, []), arc) =
			    (CON(dc, tys, []), nextArc arc)
			| cvtCase (DFA.CON(dc, tys, args), arc) = let
			    val {pvs, fvs} = infoOf arc
			    fun cvtArg (path as DFA.PATH{ty, ...}, (pmap, args)) =
				  if PSet.member(pvs, path)
				    then let
				    (* this path is tested in some subsequent state, so we
				     * need to create a fresh variable for it.
				     *)
(* QUESTION: what if there is a source variable for this path?? *)
				      val x = newVar ty
				      in
					(PMap.insert(pmap, path, x), ARG_VAR x :: args)
				      end
				    else (pmap, ARG_ANON ty::args)
			    val (pmap, args) = List.foldr cvtArg (pmap, []) args
			    in
			      (CON(dc, tys, args), next(env, pmap, arc))
			    end
		      val SOME arg = PMap.find(pmap, arg)
		      in
			CASE(arg, List.map cvtCase cases)
		      end
(* FIXME: we need bindings here! *)
		  | DFA.BIND(vmap, q) =>
		      next (extendEnvByVMap (env, pmap, vmap), pmap, q)
		  | DFA.FINAL(_, _, e) => ACTION(env, e)
		  | DFA.WHEN(vmap, e, t, f) => let
		      val env = extendEnvByVMap (env, pmap, vmap)
		      in
			IF(env, e, next(env, pmap, t), next(env, pmap, f))
		      end
		  | DFA.ERROR => ACTION(env, raiseExn(loc, env, resTy))
		(* end case *))
	  and defShared q = let
		val {pvs, fvs} = infoOf q
		val (pvars, pmap) = let
		      fun cvt (x as DFA.PATH{ty, ...}, (vars, map)) = let
			    val x' = newVar ty
			    in
			      (x'::vars, PMap.insert(map, x, x'))
			    end
		      in
			PSet.foldr cvt ([], PMap.empty) pvs
		      end
	      (* fvars are the new names for the free pattern variables of q *)
		val (fvars, env) = Env.renameList(env0, VSet.listItems fvs)
	      (* create the function *)
		val params = fvars @ pvars
		val f = newVar(TU.fnTy(List.map Var.typeOf params, [resTy]))
		val funct = (f, params, visit(env, pmap, q))
		in
		  STbl.insert stateFuns (q, funct); funct
		end
	  val fns = List.map defShared shared
	  val pmap = let
		fun f (x, pmap) = PMap.insert(pmap, DFA.ROOT x, x)
		in
		  List.foldl f PMap.empty (DFA.getArgs dfa)
		end
	  val tree = next(env0, pmap, root)
	  in
	    {fns = fns, tree = tree}
	  end

  (* get the list of variables bound by a pattern list *)
    fun bvars pats = let
	  fun bv (AST.ConPat(_, _, p), s) = bv(p, s)
	    | bv (AST.TuplePat ps, s) = bv'(ps, s)
	    | bv (AST.VarPat x, s) = VSet.add(s, x)
	    | bv (AST.ConstPat _, s) = s
	  and bv' ([], s) = s
	    | bv' (p::ps, s) = bv'(ps, bv(p, s))
	  in
	    VSet.listItems (bv'(pats, VSet.empty))
	  end

  (* given an environment that renames variables and a mapping from variables
   * to their path, extend the renaming environment.
   *)
    fun extendEnv (env, vmap, pmap) = let
	  fun ext (x, path, env) = let
		val SOME x' = PMap.find(pmap, path)
		in
		  if not(Var.same(x, x'))
		    then insertVar(env, x, x')
		    else env
		end
	  in
	    VMap.foldli ext env vmap
	  end

  (* create the AST to raise an exception (either MatchFail or BindFail) *)
    fun raiseExn getExn (loc, env, ty) = AST.E_RAISE(AST.E_CON(getExn env, loc), ty)
    val raiseMatchFail = raiseExn Env.exnMatchFail
    val raiseBindFail = raiseExn Env.exnBindFail

    fun rewrite (loc, env, exp : AST.exp) : AST.exp = let
	  fun rewrite' e = rewrite (loc, env, e)
	  in
	    case exp
	     of AST.LetExp(AST.ValBind(pat, rhs), e) => let
		  val (bind', env') = rewriteBind (loc, env, pat, rewrite' rhs)
		  in
		    AST.LetExp(bind', rewrite' e)
		  end
	      | AST.LetExp(AST.PValBind _, e) => (* should have been compiled away *)
		  raise Fail "unexpected PValBind"
	      | AST.LetExp(AST.FunBind fbs, e) => let
		  fun rewriteFB (AST.FB(f, x, e)) = AST.FB(f, x, rewrite' e)
		  in
		    AST.LetExp(AST.FunBind(List.map rewriteFB fbs), rewrite' e)
		  end
	      | AST.IfExp(e1, e2, e3, ty) =>
		  AST.IfExp(rewrite' e1, rewrite' e2, rewrite' e3, ty)
	      | AST.CaseExp(e, mc, ty) =>
		  AST.CaseExp(rewrite' e, rewriteMatch(loc, env, mc), ty)
	      | AST.ApplyExp(e1, e2, ty) => AST.ApplyExp(rewrite' e1, rewrite' e2, ty)
	      | AST.TupleExp es => AST.TupleExp(List.map rewrite' es)
	      | AST.RangeExp(e1, e2, e3, ty) =>
		  AST.RangeExp(rewrite' e1, rewrite' e2, Option.map rewrite' e3, ty)
	      | AST.PTupleExp es => AST.PTupleExp(List.map rewrite' es)
	      | AST.PArrayExp(es, ty) => AST.PArrayExp(List.map rewrite' es, ty)
	      | AST.PCompExp _ => (* should have been compiled away *)
		  raise Fail "unexpected PCompExp"
	      | AST.PChoiceExp _ => (* should have been compiled away *)
		  raise Fail "unexpected PChoiceExp"
	      | AST.SpawnExp e => AST.SpawnExp(rewrite' e)
	      | AST.ConstExp _ => exp
	      | AST.VarExp _ => exp
	      | AST.SeqExp(e1, e2) => AST.SeqExp(rewrite' e1, rewrite' e2)
	      | AST.OverloadExp(ref(AST.Instance x)) => AST.VarExp(x, [])
	      | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    (* end case *)
	  end

  (* we translate
   *    let pat = rhs;
   * to
   *    let xs = case rhs of { pat => ys | _ => raise Bind };
   * where ys are the variables bound in pat and xs are fresh copies of the
   * ys.  We also return an environment extended with ys :-> xs.  Note that
   * we assume that rhs has already been rewritten.
   *)
    and rewriteBind (loc, env, AST.TuplePat[], rhs) = (rhs, env)
      | rewriteBind (loc, env, lhs, rhs) = let
	(* rewrite the rhs expression *)
	  val rhs = rewrite (loc, env, rhs)
	(* get the variables bound by the lhs patterns *)
	  val bvs = bvars lhs
	(* synthesize a fresh variable for each pattern on the lhs *)
	  val args = let
		fun mkArg p = newVar(TypeOf.pat p)
		in
		  List.map mkArg lhs
		end
	  val resTy = TU.types(List.map Var.typeOf bvs)
	  val dfa = let
		fun mkVar x = AST.VarExp(x, [])
		in
		  MatchToDFA.rulesToDFA (
		    loc, env, args,
		    [lhs, AST.mkTupleExp[(List.map mkVar bvs)]])
		end
	  val {shared, match} = dfaToAST (loc, env, dfa, raiseBindFail, resTy)
	(* the new lhs pattern consists of copies of the bound variables *)
	  val (bvs', env') = MatchCompEnv.renameList (env, bvs)
	(* construct the list of statments *)
	  val stms =
		AST.S_VAL(List.map AST.P_VAR args, rhs) ::
		shared @
		[AST.S_VAL(List.map AST.P_VAR bvs', match)]
	  in
	  (* check for nonexhaustive binding *)
	    if (DFA.errorCount dfa <> 0)
	      then Err.warnNonexhaustiveBind(Env.errStrm env, loc)
	      else ();
	  (* we return a list of statements and the extended environment *)
	    (stms, env')
	  end

  (* rewrite a case match. *)
    and rewriteMatch (loc, env, AST.MCASE{argTy, resTy, cases}) = let
	  val argTys = TU.toTypes argTy
	  val args = List.map newVar argTys
	  val dfa = MatchToDFA.rulesToDFA (loc, env, args, cases)
	  val {shared, match} = dfaToAST (loc, env, dfa, raiseMatchFail, resTy)
	(* Filter out unreachable default states. *)
	  val final = List.filter (not o DFA.unusedDefault)
		(DFA.finalStates dfa)
	  in
(* NOTE: perhaps we should issue an error message for each
 * redundant match with more precise location information???
 *)
	  (* check for redundant matches; note that M_DEFAULT cannot cause
	   * a redundant match, since it would have been filtered out above.
	   *)
	    if (List.exists (fn q => DFA.rCount q = 0) final)
	      then Err.errRedundantMatch(Env.errStrm env, loc)
	      else ();
	   (* check for nonexhaustive match *)
	    if (DFA.errorCount dfa <> 0)
	      then Err.warnNonexhaustiveMatch(Env.errStrm env, loc)
	      else ();
	    case shared
	     of [] => AST.MCASE{
		  argTy=argTy, resTy=resTy,
		  cases = [AST.mkTuplePat(List.map AST.VarPat args), match]
		}
	      | _ => AST.MCASE{
		  argTy=argTy, resTy=resTy,
		  cases = [
		      AST.M_MATCH(List.map AST.P_VAR args,
			AST.E_BLOCK(shared @ [AST.S_EXP match], resTy))
		    ]
		}
	    (* end case *)
 	  end

  (* convert the DFA representation back to typed AST.  The result of this
   * function is a list of functions that represent the shared states of the
   * DFA and an expression that is the simplified match case.
   *)
    and dfaToAST (loc, env, dfa, raiseExn, resTy) = let
(* +DEBUG *)
	  val _ = if !MatchOptions.debugFlg
		then DFA.dump (TextIO.stdOut, dfa)
		else ()
(* -DEBUG *)
	  val {fns, tree} = dfaToTrees (loc, env, dfa, raiseExn, resTy)
	(* create a variable expression *)
(* FIXME: what about type arguments?? *)
	  fun mkVar x = AST.VarExp(x, [])
	(* convert a decision tree to TypedAST *)
	  fun treeToAST (CALL(f, args)) = AST.E_APP{
		  f = mkVar f, args = List.map mkVar args,
		  resTy = resTy, loc = loc
		}
	    | treeToAST (CASE(x, cases)) = let
		val argTy = Var.typeOf x
		fun cvtCase (pat, t) = let
		      fun cvtArg (ARG_ANON ty) = AST.P_WILD ty
			| cvtArg (ARG_VAR x) = AST.P_VAR x
		      val pat = (case pat
			     of WILD => AST.VarPat(Var.new("_", argTy))
			      | VAR x => AST.VarPat x
			      | LIT lit => AST.ConstPat(AST.LConst(lit, argTy))
			      | CON(dc, tys, []) => AST.ConstPat(AST.DConst(dc, tys))
			      | CON(dc, tys, args) =>
				  AST.ConPat(dc, tys, List.map cvtArg args)
			    (* end case *))
		      in
			(pat, treeToAST t)
		      end
		in
		  AST.CaseExp(mkVar x, List.map cvtCase cases, resTy)
		end
	    | treeToAST (IF(env, cond, t, f)) =
		AST.IfExp(rewrite(loc, env, cond), treeToAST t, treeToAST f, resTy)
	    | treeToAST (ACTION(env, e)) = rewrite(loc, env, e)
	  fun treeToFB (f, params, body) = (f, [], params, treeToAST body)
	  val shared = (case fns
		 of [] => []
		  | _ => [AST.S_FUN(List.map treeToFB fns)]
		(* end case *))
	  in
	    { shared = shared, match = treeToAST tree }
	  end

    fun compile (exp : AST.module) = rewrite (?, ?, exp)

  end
