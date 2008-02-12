(* match-compile.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Translate bindings and matches to simple matches.
 *)

structure MatchCompile : sig

  (* expand bindings and pattern matches to simplified form *)
    val compile : Error.err_stream * AST.module -> AST.module

  end = struct

    structure L = Literal
    structure Ty = Types
    structure TU = TypeUtil
    structure DFA = MatchDFA
    structure VSet = Var.Set
    structure VMap = Var.Map
    structure Env = MatchCompEnv
    structure Err = MatchErrors

  (* Debugging support *)
    fun dumpAST (outFile, ast) = if Controls.get MatchControls.keepAST
	  then let
	    val outStrm = TextIO.openOut outFile
	    in
	      PrintAST.output (outStrm, ast);
	      TextIO.closeOut outStrm
	    end
	  else ()

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
		  | DFA.FINAL(fvs, _) => {pvs = PSet.empty, fvs = fvs}
		  | DFA.COND(vmap, _, arc1, arc2) => let
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


    type env = Env.env

    val insertVar = Env.insert
    val applyEnv = Env.apply

  (* extend the renaming environment env by the composition of
   * vmap, which maps variables to paths, with pmap, which maps
   * paths to variables.
   *)
    fun extendEnv (env, vmap, pmap) = let
	  fun ext (x, path, env) = (case PMap.find(pmap, path)
		 of SOME x' =>
		      if not(Var.same(x, x'))
			then insertVar(env, x, x')
			else env
		  | NONE => raise Fail(concat[
			"ext(", Var.toString x, ", ",
			DFA.pathToString path, "): no binding for path"
		      ])
		(* end case *))
	  in
	    VMap.foldli ext env vmap
	  end

    datatype decision_tree
      = CALL of AST.var * AST.var list
      | CASE of (AST.var * (simple_pat * decision_tree) list)
      | IF of (env * AST.exp * decision_tree * decision_tree)
      | ACTION of (env * AST.exp)

    and simple_pat
      = WILD
      | VAR of AST.var
      | LIT of L.literal
      | TPL of pat_arg list
      | CON of (AST.dcon * Ty.ty list * pat_arg list)

    and pat_arg
      = ARG_ANON of Ty.ty
      | ARG_VAR of AST.var

(* +DEBUG *)
    fun prTree tr = let
	  val pr = TextIO.print
	  fun indent 0 = ()
	    | indent i = (pr "  "; indent(i-1))
	  fun tree (i, nd) = (
		case nd
		 of CALL(f, args) => (
		      pr (Var.toString f); pr " (";
		      pr (String.concatWith "," (List.map Var.toString args));
		      pr ")")
		  | CASE(x, cases) => let
		      fun argToString (ARG_ANON _) = "_"
			| argToString (ARG_VAR x) = Var.toString x
		      fun prArgs args = (pr "("; pr(String.concatWith "," (List.map argToString args)); pr ")")
		      fun prPat WILD = pr "_"
			| prPat (VAR x) = pr(Var.toString x)
			| prPat (LIT lit) = pr(Literal.toString lit)
			| prPat (TPL args) = prArgs args
			| prPat (CON(dc, _, [])) = pr (DataCon.nameOf dc)
			| prPat (CON(dc, _, args)) = (pr (DataCon.nameOf dc); prArgs args)
		      fun prCase (prefix, (pat, tr)) = (
			    indent i; pr prefix; pr " "; prPat pat; pr " => ";
			    tree (i+1, tr))
		      in
			pr "CASE "; pr(Var.toString x); pr "\n";
			prCase (" OF", hd cases);
			List.app (fn c => (pr "\n"; prCase("  |", c))) (tl cases)
		      end
		  | IF(_, _, t1, t2) => (
		      pr "IF <exp>\n";
		      indent(i+1); pr "THEN "; tree(i+2, t1); pr "\n";
		      indent(i+1); pr "ELSE "; tree(i+2, t2))
		  | ACTION _ => pr "ACTION"
		(* end case *))
	  in
	    tree (0, tr); pr "\n"
	  end
(* -DEBUG *)

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
		      fun cvtArg pvs (path as DFA.PATH{ty, ...}, (pmap, args)) =
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
		      fun cvtCase (DFA.ANY, arc) = (case PMap.find(pmap, arg)
			     of NONE => (WILD, nextArc arc)
			      | SOME x => (VAR x, nextArc arc)
			    (* end case *))
			| cvtCase (DFA.LIT lit, arc) = (LIT lit, nextArc arc)
			| cvtCase (DFA.TPL args, arc) = let
			    val {pvs, fvs} = infoOf arc
			    val (pmap, args) = List.foldr (cvtArg pvs) (pmap, []) args
			    in
			      (TPL args, next(env, pmap, arc))
			    end
			| cvtCase (DFA.CON(dc, tys, []), arc) =
			    (CON(dc, tys, []), nextArc arc)
			| cvtCase (DFA.CON(dc, tys, [arg]), arc) = let
			    val {pvs, fvs} = infoOf arc
			    val (pmap, args) = cvtArg pvs (arg, (pmap, []))
			    in
			      (CON(dc, tys, args), next(env, pmap, arc))
			    end
		      val SOME arg = PMap.find(pmap, arg)
		      in
			CASE(arg, List.map cvtCase cases)
		      end
(* FIXME: we need bindings here! *)
		  | DFA.BIND(vmap, q) =>
		      next (extendEnv (env, vmap, pmap), pmap, q)
		  | DFA.FINAL(_, e) => ACTION(env, e)
		  | DFA.COND(vmap, e, t, f) => let
		      val env = extendEnv (env, vmap, pmap)
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
		val f = newVar(TU.funTy(List.map Var.monoTypeOf params, [resTy]))
		val funct = (f, params, visit(env, pmap, q))
		in
		  STbl.insert stateFuns (q, funct); funct
		end
	  val fns = List.map defShared shared
	  val pmap = PMap.singleton (DFA.ROOT(DFA.getArg dfa), DFA.getArg dfa)
	  val tree = next(env0, pmap, root)
	  in
	    {fns = fns, tree = tree}
	  end

  (* create the AST to raise an exception (either MatchFail or BindFail) *)
(*
    fun raiseExn getExn (loc : Err.span, env : Env.env, ty : AST.ty) = AST.E_RAISE(AST.E_CON(getExn env, loc), ty)
    val raiseMatchFail = raiseExn Env.exnMatchFail
    val raiseBindFail = raiseExn Env.exnBindFail
*)
    fun raiseExn exn (loc : Err.span, env : Env.env, ty : AST.ty) =
	  AST.ApplyExp(
	    AST.VarExp(Basis.fail, [ty]),
	    AST.ConstExp(AST.LConst(Literal.String exn, Basis.stringTy)),
	    ty)
    val raiseMatchFail = raiseExn "Match"
    val raiseBindFail = raiseExn "Bind"

    fun rewrite (loc, env, exp : AST.exp) : AST.exp = let
	  fun rewrite' e = rewrite (loc, env, e)
	  in
	    case exp
	     of AST.LetExp(AST.ValBind(pat, rhs), e) =>
		  if MatchUtil.isSimplePat pat
		    then AST.LetExp(AST.ValBind(pat, rewrite' rhs), rewrite' e)
		    else let
		      val (binds, env') = rewriteBind (loc, env, pat, rewrite' rhs)
		      in
			ASTUtil.mkLetExp(binds, rewrite(loc, env', e))
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
		  if MatchUtil.areSimpleMatches mc
		    then let
		      fun f (AST.PatMatch(p, e)) = AST.PatMatch(p, rewrite' e)
		        | f _ = raise Fail "impossible"
		      in
			AST.CaseExp(rewrite' e, List.map f mc, ty)
		      end
		    else AST.CaseExp(rewrite' e, rewriteMatch(loc, env, TypeOf.exp e, mc, ty), ty)
	      | AST.HandleExp(e, mc, ty) => raise Fail "handle" (* FIXME *)
	      | AST.RaiseExp(e, ty) => AST.RaiseExp(rewrite' e, ty)
	      | AST.FunExp(x, e, ty) => AST.FunExp(x, rewrite' e, ty)
	      | AST.ApplyExp(e1, e2, ty) => AST.ApplyExp(rewrite' e1, rewrite' e2, ty)
	      | AST.VarArityOpExp _ => exp
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
	      | AST.VarExp(x, tys) => AST.VarExp(applyEnv(env, x), tys)
	      | AST.SeqExp(e1, e2) => AST.SeqExp(rewrite' e1, rewrite' e2)
	      | AST.OverloadExp(ref(AST.Instance x)) => AST.VarExp(x, [])
	      | AST.OverloadExp _ => raise Fail "unresolved overloading"
	    (* end case *)
	  end

  (* we translate
   *    val pat = rhs;
   * to
   *    val xs = case rhs of pat => ys | _ => raise Bind
   * where ys are the variables bound in pat and xs are fresh copies of the
   * ys.  We also return an environment extended with ys :-> xs.  Note that
   * we assume that rhs has already been rewritten.
   *)
    and rewriteBind (loc, env, lhs, rhs) = let
	(* get the variables bound by the lhs patterns *)
	  val bvs = Var.Set.listItems (MatchUtil.varsOfPat lhs)
(* do we really need a variable for the lhs? *)
	(* synthesize a fresh variable for the lhs *)
	  val arg = newVar(TypeOf.pat lhs)
	  val resTy = TU.tupleTy(List.map Var.monoTypeOf bvs)
	  val dfa = let
		fun mkVar x = AST.VarExp(x, [])
		in
		  MatchToDFA.rulesToDFA (
		    loc, arg,
		    [AST.PatMatch(lhs, ASTUtil.mkTupleExp(List.map mkVar bvs))])
		end
	  val {shared, match} = dfaToAST (loc, env, dfa, raiseBindFail, resTy)
	(* the new lhs pattern consists of copies of the bound variables *)
	  val (bvs', env') = Env.renameList (env, bvs)
	(* construct the binding *)
	  val binds = (case shared
		 of NONE => [
			AST.ValBind(AST.VarPat arg, rhs),
			AST.ValBind(ASTUtil.mkTuplePat(List.map AST.VarPat bvs'), match)
		      ]
		  | SOME fb => [
			AST.ValBind(AST.VarPat arg, rhs),
			fb,
			AST.ValBind(ASTUtil.mkTuplePat(List.map AST.VarPat bvs'), match)
		      ]
		(* end case *))
	  in
	  (* check for nonexhaustive binding *)
	    if (DFA.errorCount dfa <> 0)
	      then Err.warnNonexhaustiveBind(Env.errStrm env, loc)
	      else ();
	  (* we the rewriten binding and the extended environment *)
	    (binds, env')
	  end

  (* rewrite a case match. *)
    and rewriteMatch (loc, env, argTy, cases, resTy) = let
	  val arg = newVar argTy
	  val dfa = MatchToDFA.rulesToDFA (loc, arg, cases)
	  val {shared, match} = dfaToAST (loc, env, dfa, raiseMatchFail, resTy)
	  val final = DFA.finalStates dfa
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
	     of NONE => [AST.PatMatch(AST.VarPat arg, match)]
	      | SOME fb => [AST.PatMatch(AST.VarPat arg, AST.LetExp(fb, match))]
	    (* end case *)
 	  end

  (* convert the DFA representation back to typed AST.  The result of this
   * function is a list of functions that represent the shared states of the
   * DFA and an expression that is the simplified match case.
   *)
    and dfaToAST (loc, env, dfa, raiseExn, resTy) : {shared : AST.binding option, match : AST.exp} = let
(* +DEBUG *)
	  val debug = Controls.get MatchControls.debug
	  val _ = if debug
		then DFA.dump (TextIO.stdOut, dfa)
		else ()
(* -DEBUG *)
	  val {fns, tree} = dfaToTrees (loc, env, dfa, raiseExn, resTy)
	(* create a variable expression *)
(* FIXME: what about type arguments?? *)
	  fun mkVar x = AST.VarExp(x, [])
	(* flatten applications of constructors to tuples *)
	  fun flattenTree tr = let
		fun flatten tr = (case tr
		       of CALL _ => tr
			| CASE(x, cases) => let
			    fun flattenCase (pat as CON(dc, tys, [ARG_VAR y]), tr as CASE(z, [(TPL args, tr')])) =
				  if (Var.same(y, z))
				    then (CON(dc, tys, args), flatten tr')
				    else (pat, flatten tr)
			      | flattenCase (pat, tr) = (pat, flatten tr)
			    in
			      CASE(x, List.map flattenCase cases)
			    end
			| IF(env, e, tr1, tr2) => IF(env, e, flatten tr1, flatten tr2)
			| ACTION _ => tr
		      (* end case *))
		val _ = if debug
		      then (print "** Decision tree before flattening:\n"; prTree tr)
		      else ()
		val tr = flatten tr
		val _ = if debug
		      then (print "** Decision tree after flattening:\n"; prTree tr)
		      else ()
		in
		  tr
		end
	(* convert a decision tree to TypedAST *)
	  fun treeToAST (CALL(f, args)) =
		AST.ApplyExp(mkVar f, ASTUtil.mkTupleExp(List.map mkVar args), resTy)
	    | treeToAST (CASE(x, cases)) = let
		val argTy = Var.monoTypeOf x
		fun cvtCase (pat, t) = let
		      fun cvtArg (ARG_ANON ty) = AST.WildPat ty
			| cvtArg (ARG_VAR x) = AST.VarPat x
		      val pat = (case pat
			     of WILD => AST.WildPat argTy
			      | VAR x => AST.VarPat x
			      | LIT lit => AST.ConstPat(AST.LConst(lit, argTy))
			      | TPL args => AST.TuplePat(List.map cvtArg args)
			      | CON(dc, tys, []) => AST.ConstPat(AST.DConst(dc, tys))
			      | CON(dc, tys, [arg]) => AST.ConPat(dc, tys, cvtArg arg)
			      | CON(dc, tys, args) =>
				  AST.ConPat(dc, tys, AST.TuplePat(List.map cvtArg args))
			    (* end case *))
		      in
			AST.PatMatch(pat, treeToAST t)
		      end
		in
		  AST.CaseExp(mkVar x, List.map cvtCase cases, resTy)
		end
	    | treeToAST (IF(env, cond, t, f)) =
		AST.IfExp(rewrite(loc, env, cond), treeToAST t, treeToAST f, resTy)
	    | treeToAST (ACTION(env, e)) = rewrite(loc, env, e)
	  fun treeToFB (f, params, body) =
		ASTUtil.mkFunWithParams(f, params, treeToAST (flattenTree body))
	  val shared = (case fns
		 of [] => NONE
		  | _ => SOME(AST.FunBind(List.map treeToFB fns))
		(* end case *))
	  in
	    { shared = shared, match = treeToAST (flattenTree tree) }
	  end

    fun compile (errStrm, module as AST.Module{exns, body}) = let
	  val base = (case Controls.get BasicControl.keepPassBaseName
		 of NONE => "match-comp"
		  | SOME fname => fname ^ ".match-comp"
		(* end case *))
	  val _ = dumpAST (base ^ ".pre.ast", module)
	  val exp = rewrite ((0, 0), Env.new errStrm, body)
	  val module' = AST.Module{exns = exns, body = exp}
	  in
	    dumpAST (base ^ ".post.ast", module');
	    module'
	  end

  end
