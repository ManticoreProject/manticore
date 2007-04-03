(* convert.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Convert the BOM representation to CPS.
 *)

structure Convert : sig

    val transform : BOM.module -> CPS.module

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure C = CPS
    structure CV = C.Var
    structure CTy = CPSTy
    structure E = BV.Map

  (* convert a BOM type to a CPS type *)
    fun cvtTy BTy.T_Any = CTy.T_Any
      | cvtTy (BTy.T_Enum w) = CTy.T_Enum w
      | cvtTy (BTy.T_Raw rTy) = CTy.T_Raw rTy
      | cvtTy (BTy.T_Wrap rTy) = CTy.T_Wrap rTy
      | cvtTy (BTy.T_Tuple tys) =
      | cvtTy (BTy.T_Fun(paramTys, exhTy, retTy) = let
	  val retTy' = cvtTy retTy
	  val retKTy' = CTy.contTy[retTy']
	  in
	    CTy.T_Fun(List.map cvtTy paramTys, [retKTy', cvtTy exhTy])
	  end
      | cvtTy (BTy.T_Cont tys) = CTy.contTy(List.map cvtTy tys)

  (* create a new CPS variable using the name of a BOM variable *)
    fun newVar (v, ty') = CV.new(BV.nameOf v, ty')

  (* create a new CPS variable using the name and type of a BOM variable *)
    fun cvtVar v = newVar (v, cvtTy(BV.typeOf v))

    fun bindVar (env, v) = let
	  val v' = cvtVar v
	  in
	    (v', E.insert(env, v, v'))
	  end

    fun bindVars (env, [], vs') = (List.rev vs', env)
      | bindVars (env, v::vs, vs') = let
	  val (v', env) = bindVar(env, v)
	  in
	    bindVars(env, vs, v'::vs')
	  end

    fun bindLambda (B.FB{f, ...}, env) = #2(bindVar (env, f))

    fun lookup (env, s) = (case E.find(env, x)
	   of SOME x' => x'
	    | NONE => raise Fail("unbound variable " ^ BV.toString x)
	  (* end case *))
    fun lookupVars (env, xs) = List.map (fn x => lookup(env, x)) xs

    fun cvtLambda (env, B.FB{f, params, exh, body}) = let
	  val f' = lookup(env, f)
	  val (params', env) = bindVars (env, params)
	  val retK' = CV.new("retK", CTy.returnTy(CV.typeOf f'))
	  val (exh', env) = bindVar (env, exh)
	  val body' = cvtExpTail(env, body, retK')
	  in
	    C.FB{
		f = f', params = params', rets = [retK', exh'],
		body = body'
	      }
	  end

  (* convert an expression that is in a tail position *)
    and cvtTailE (env, B.E_Pt(_, e), retK') = (case e
	   of B.E_Let(xs, e1, e2) => let
		val (xs', env2) = cvtVars (env, xs)
		val tys' = List.map CV.typeOf xs'
		val joinK' = CV.new("letJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=xs', rets=[], body=cvtTailE(env2, e2, retK')},
		    cvtE (env, e1, tys', fn k ys' => C.Throw(joinK', ys')))
		end
	    | B.E_Stmt(xs, rhs, e) =>
		cvtRHS (env, xs, rhs, fn env => cvtTailE(env, e, retK'))
	    | B.E_Fun(fbs, e) =>
		cvtFun (env, fbs,
		  fn (env2, fbs') => C.mkFun(fbs', cvtTailE(env2, e, retK')))
	    | B.E_Cont(kb, e) =>
		cvtCont (env, fb,
		  fn (env2, fb') => C.mkCont(fb', cvtTailE(env2, e, retK')))
	    | B.E_If(x, e1, e2) =>
		C.If(lookup(env, x),
		  cvtTailE (env, e1, retK'),
		  cvtTailE (env, e2, retK'))
	    | B.E_Case(x, cases, optDflt) => cvtCase (env, x, cases, optDflt, retK')
	    | B.E_Apply(f, params, exh) =
		C.Apply(lookup(env, f),
		  lookupVars(env, params),
		  [retK', lookupVar(env, exh)])
	    | B.E_Throw(k, xs) => C.Throw(lookup(env, k), lookupVars(env, xs))
	    | B.E_Ret xs => C.Throw(retK', lookupVars(env, xs))
	  (* end case *))

  (* convert an expression that is in a non-tail position; the tys' parameter is
   * a list of the result types returned by the expression.
   *)
    and cvtE (env, B.E_Pt(_, e), tys', k) = (case e
	   of B.E_Let(xs, e1, e2) => let
		val (xs', env2) = cvtVars (env, xs)
		val tys2' = List.map CV.typeOf xs'
		val joinK' = CV.new("letJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=xs', rets=[], body=cvtE(env2, e2, tys', retK')},
		    cvtE (env, e1, tys2', fn k ys' => C.Throw(joinK', ys')))
		end
	    | B.E_Stmt(xs, rhs, e) =>
		cvtRHS (env, xs, rhs, fn env => cvtE(env, e, tys', k))
	    | B.E_Fun(fbs, e) =>
		cvtFun (env, fbs,
		  fn (env2, fbs') => C.mkFun(fbs', cvtE(env2, e, tys', k)))
	    | B.E_Cont(kb, e) =>
		cvtCont (env, fb,
		  fn (env2, fb') => C.mkCont(fb', cvtE(env2, e, tys', k)))
	    | B.E_If(x, e1, e2) => let
		val ys' = List.map (fn ty => CV.new("a", ty)) tys'
		val joinK' = CV.new("ifJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=ys', rets=[], body=k ys'},
		    C.If(lookup(env, x),
		      cvtTailE(env, joink'),
		      cvtTailE(env, joink')))
		end
	    | B.E_Case(x, cases, optDflt) => let
		val ys' = List.map (fn ty => CV.new("a", ty)) tys'
		val joinK' = CV.new("caseJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=ys', rets=[], body=k ys'},
		    cvtCase (env, x, cases, optDflt, joinK'))
		end
	    | B.E_Apply(f, params, exh) = let
		val f' = lookup(env, f)
		val params' = lookupVars(env, params)
		val exh' = lookup(env, exh)
		val retK' = CV.new("retK", CTy.returnTy(CV.typeOf f'))
		val a' = CV.new("a", ??)
		in
		  C.mkCont(
		    C.FB{f=retK', params=[a'], rets=[], body=k [a']},
		    C.Apply(f', params', [retK', ehx']))
		end
	    | B.E_Throw(k, xs) => C.Throw(lookup(env, k), lookupVars(env, xs))
	    | B.E_Ret xs = k(lookupVars(env, xs))
	  (* end case *))

    and cvtRHS (env, lhs, rhs, k) = let
	  fun cv x = lookup(env, x)
	  val (lhs', env) = bindVars (env, lhs)
	  val rhs' = (case rhs
		 of B.E_Const c => (case c
		       of B.E_EnumConst(w, _) => C.Enum w
			| B.E_IConst(n, _) => C.Literal(Literal.Int n)
			| B.E_SConst s => C.Literal(Literal.String s)
			| B.E_FConst(f, _) => C.Literal(Literal.Float f)
			| B.E_BConst b => C.Literal(Literal.Bool b)
		      (* end case *))
		  | B.E_Cast(ty, x) => C.Cast(cvtTy ty, cv x)
		  | B.E_Select(i, x) => C.Select(i, cv x)
		  | B.E_Alloc(ty, args) => C.Alloc(List.map cv args)
		  | B.E_Wrap x => C.Wrap(cv x)
		  | B.E_Unwrap x => C.Unwrap(cv x)
		  | B.E_Prim p => C.Prim(PrimUtil.map cv p)
		  | B.E_DCon _ => raise Fail "unexpected DCon"
		  | B.E_HLOp _ => raise Fail "unexpected HLOp"
		  | B.E_CCall(f, args) => C.CCall(cv f, List.map cv args)
		  | B.E_QItemAlloc xs => raise Fail "QItemAlloc unimplemented"
		  | B.E_QEnqueue(x, y) => raise Fail "QEnqueue unimplemented"
		  | B.E_QDequeue x => raise Fail "QDequeue unimplemented"
		  | B.E_QEmpty x => raise Fail "QEmpty unimplemented"
		  | B.E_AtomicQEnqueue(x, y) => raise Fail "AtomicQEnqueue unimplemented"
		  | B.E_AtomicQDequeue x => raise Fail "AtomicQDequeue unimplemented"
		(* scheduler operations *)
		  | B.E_Dequeue x => C.Dequeue(cv x)
		  | B.E_Enqueue(x, y, z) => C.Enqueue(cv x, cv y, cv z)
		(* VProc operations *)
		  | B.E_HostVProc => C.HostVProc
		  | B.E_VPLoad(off, x) => C.VPLoad(off, cv x)
		  | B.E_VPStore(off, x, y) => C.VPStore(off, cv x, cv y)
		(* end case *))
	  in
	    C.mkLet(lhs', rhs', k env2)
	  end

  (* convert the body of a case expression; note that after case simplification, all cases
   * will just involve enumeration tags.
   *)
    and cvtCase (env, x, cases, optDflt, retK') = let
	  fun cvtCase (B.P_Const(P.EnumConst(tag, _)), e) = (tag, cvtTailE(env, e, retK'))
	    | cvtCase c = raise Fail "complex case"
	  val dflt' = Option.map(fn e => cvtTailE(env, e, retK')) dflt
	  in
	    C.Switch(lookup(env, x), List.map cvtCase cases, dflt')
	  end

    and cvtFun (env, fbs, k) = let
	  val env = List.foldl bindLambda env fbs
	  val fbs' = List.map (fn fb => cvtLambda(env, fb)) fbs
	  in
	    k (env, fbs')
	  end

    and cvtCont (env, fb, k) = let
	  val env = bindLambda(fb, env)
	  val fb' = cvtLambda(env, fb)
	  in
	    k (env, fb')
	  end

  end
