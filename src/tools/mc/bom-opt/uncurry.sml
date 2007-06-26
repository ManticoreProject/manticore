(* uncurry.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An implementation of Tarditi's uncurrying optimization for BOM.
 *)

structure Uncurry : sig

    val noUncurryFlg : bool ref

    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BTy = BOMTy
    structure BV = BOM.Var
    structure VTbl = BV.Tbl
    structure PTbl = ProgPt.Tbl
    structure C = Census
    structure ST = Stats

    val noUncurryFlg = ref false

    val cntReplace		= ST.newCounter "uncurry:replace-apply"
    val cntElim			= ST.newCounter "uncurry:elim-apply"

  (***** Analysis *****
   *
   * The analysis phase both finds curried functions and the detects partial application sites.
   *)

    datatype curried_fn	    (* information about a curried function *)
      = DEF of int		(* a curried function; the integer (>1) is the number *)
				(* of curried arguments *)
      | PARTIAL of {		(* the partial application of a curried function *)
	    arity : int,	    (* the remaining depth of currying (>= 0) *)
	    f : B.var,		    (* the original curried function *)
	    args : B.var list list, (* the arguments for each partial application (in right to *)
				    (* left order) *)
	    exh : B.var list	    (* the exception-handler continuation from the rightmost *)
				    (* application (the other applications cannot raise exceptions) *)
	  }

    fun analyse (_, _, B.FB{body, ...}) = let
	(* this flag is set to true if there are opportunities for the uncurrying transformation *)
	  val optPossible = ref false
	(* partial map from functions to currying info *)
	  val curriedFns = VTbl.mkTable (16, Fail "CurriedFns")
	(* record the currying depth n of the function f *)
	  fun setArity (f, n) = VTbl.insert curriedFns (f, DEF n)
	(* lookup information about a curried function; returns NONE for non-curried functions *)
	  val findCurried = VTbl.find curriedFns
	(* record that the variable g is bound to a partial application of the curried function f *)
	  fun recordPartial (g, n, f, args, exh) = (
		if (n = 1) then optPossible := true else ();
		VTbl.insert curriedFns (g, PARTIAL{arity=n-1, f=f, args=args, exh=exh}))
	(* compute and record the curried arity (i.e., # of levels of currying) for
	 * a function.  A level of currying is a function body of the form
	 *
	 *    fun f x = e in f
	 *
	 * where f does not occur free in e.  We check the latter property
	 * by testing the use count of f (which will be > 1 when f is free in e).
	 *)
	  fun chkFB (f0, _, body) = let
		fun arity (n, e as B.E_Pt(_, t)) = (case t
		       of B.E_Fun([B.FB{f, body, ...}], B.E_Pt(_, B.E_Ret[f'])) =>
			    if BV.same(f, f') andalso (BV.useCntOf f = 1)
			      then arity(n+1, body)
			      else done (n, body)
			| _ => done (n, e)
		      (* end case *))
		and done (1, e) = chkExp e
		  | done (n, e) = (
		      setArity(f0, n);
		      chkExp e)
		in
		  arity (1, body)
		end
	  and chkFBs fbs = List.app chkFB fbs
	  and chkExp (B.E_Pt(_, t)) = (case t
		 of B.E_Let([g], B.E_Pt(_, B.E_Apply(f, xs, ys)), e) => (
		      case findCurried f
		       of NONE => ()
			| SOME(DEF n) => recordPartial(g, n, f, [xs], ys)
			| SOME(PARTIAL{arity=0, ...}) => raise Fail "bogus application"
			| SOME(PARTIAL{arity, f, args, ...}) =>
			    recordPartial(g, arity, f, xs::args, exh=ys)
		      (* end case *);
		      chkExp e)
		  | B.E_Let(lhs, rhs, e) => (chkExp rhs; chkExp e)
		  | B.E_Stmt(_, _, e) => chkExp e
		  | B.E_Fun(fbs, e) => (chkFBs fbs; chkExp e)
		  | B.E_Cont(B.FB{body, ...}, e) => (chkExp body; chkExp e)
		  | B.E_If(_, e1, e2) => (chkExp e1; chkExp e2)
		  | B.E_Case(_, cases, dflt) => (
		      List.app (chkExp o #2) cases;
		      Option.app chkExp dflt)
		  | B.E_Apply(f, _, _) => (case findCurried f
		       of SOME(PARTIAL{arity=1, ...}) => optPossible := true
			| _ => ()
		      (* end case *))
		  | B.E_Throw _ => ()
		  | B.E_Ret _ => ()
		  | B.E_HLOp _ => ()
		(* end case *))
	  in
	    chkExp body;
	    (!optPossible, findCurried)
	  end

    fun xform (B.MODULE{name, externs, body}) = let
	  val (optPossible, findCurried) = analyse body
	  val uncurried = VTbl.mkTable (16, Fail "UncurriedFns")
	(* lookup the curry-arity of the function *)
	  fun arityOf f = (case findCurried f
		 of SOME(DEF n) => n
		  | _ => 1
		(* end case *))
	  fun mkApply (f, allArgs, exh) = let
		val f = VTbl.lookup uncurried f
		val args = List.foldl (op @) [] allArgs
		in
		  C.incAppCnt f;
		  List.app C.incUseCnt args;
		  List.app C.incUseCnt exh;
		  B.mkApply (f, args, exh)
		end
(*DEBUG*)handle ex => (print(concat["mkApply(", BV.toString f, ", _, _)\n"]); raise ex)
	  fun xformFB (B.FB{f, params, exh, body}, fbs) = let
		val arity = arityOf f
		fun copyParam x = let val x' as B.V{useCnt, ...} = BV.copyVar x
		      in
			useCnt := 1;
			x'
		      end
		in
		  if arity > 1
		    then let
		      fun flatten (g, 0, allParams, newParams, body) = let
			    val bty = BTy.T_FunPtr{
				    dom = List.map BV.typeOf allParams,
				    rng = BTy.rangeOf(BV.typeOf g)
				  }
			    val f' = BV.aliasVar(f, SOME "_uncurried", bty)
			    in
			      C.incAppCnt f';
			      (B.mkApply(f', newParams), C.mkLambda(f', allParams, body))
			    end
			| flatten (_, n, allParams, newParams, fb) = let
			    val B.E_Pt(_, B.E_Fun([(g, params, body)], e)) = fb
			    val params' = List.map copyParam params
			    val (body', newFB) = flatten(
				  g, n-1,
				  allParams @ params,
				  newParams @ params',
				  body)
			    in
			      (C.mkFun([(g, params', body')], xformExp e), newFB)
			    end
		      val params' = List.map copyParam params
		      val (curriedFun, uncurriedFB) =
			    flatten (f, arity-1, params, params', body)
		      in
			VTbl.insert uncurried (f, #1 uncurriedFB);
			C.mkLambda(f, params', curriedFun) :: uncurriedFB :: fbs
		      end
		    else C.mkLambda(f, params, xformExp body) :: fbs
		end
	  and xformFBs fbs = List.foldr xformFB [] fbs
	  and xformExp (e as B.E_Pt(_, t)) = (case t
		 of B.E_Let([g], rhs as B.E_Pt(_, B.E_Apply(f, args)), e) => (
		      case findCurried g
		       of SOME(PARTIAL(0, h, allArgs)) => (
			    ST.tick cntReplace;
			    C.decAppCnt f;
			    List.app C.decUseCnt args;
			    C.mkLet([g], mkApply(h, allArgs), xformExp e))
			| SOME _ => let
			    val e = xformExp e
			    in
			      if BV.useCntOf g > 0
				then B.mkLet([g], rhs, e)
				else (
				  ST.tick cntElim;
				  C.decUseCnt g;
				  C.decAppCnt f;
				  List.app C.decUseCnt args;
				  e)
			    end
			| NONE => B.mkLet([g], rhs, xformExp e)
		      (* end case *))
		  | B.E_Let(lhs, rhs, e) => C.mkLet(lhs, xformExp rhs, xformExp e)
		  | B.E_Stmt(lhs, rhs, e) => C.mkStmt(lhs, rhs, xformExp e)
		  | B.E_Fun(fbs, e) =>
		    (* NOTE: xformFBs takes care of census info for the fbs *)
		      B.mkFun(xformFBs fbs, xformExp e)
		  | B.E_Cont(B.FB{f=k, params, exh, body}, e) =>
		      C.mkCont(
			B.FB{f=k, params=params, exh=exh, body=xformExp e1},
			xformExp e)
		  | B.E_If(x, e1, e2) =>
		      B.mkIf(x, xformExp e1, xformExp e2)
		  | B.E_Case(x, cases, dflt) =>
		      B.mkCase(x,
			List.map (fn (l, e) => (l, xformExp e)) cases,
			Option.map xformExp dflt)
		  | B.E_Apply(f, args, rets) => (case findCurried f
		       of SOME(PARTIAL(1, h, allArgs, rets)) => (
			    ST.tick cntReplace;
			    C.decAppCnt f;
			    List.app C.decUseCnt args; (* will get inc by mkApply *)
			    mkApply(h, args::allArgs, rets))
			| _ => e
		      (* end case *))
		  | B.E_Throw _ => e
		  | B.E_Ret _ => e
		  | B.E_HLOp _ => e
		(* end case *))
	  in
	    if optPossible
	      then let
		val B.FB{f, params, exh, body} = body
		in
		  B.mkModule(name, externs, B.FB{f=f, params=params, exh=exh, body=xformExp body})
		end
	      else module
	  end

    fun transform module = if !noUncurryFlg
	  then module
	  else xform module

  end (* Uncurry *)
