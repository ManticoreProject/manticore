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
    structure C = CPS
    structure CV = C.Var
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

    fun bindLambda (B.FB{f, ...}, env) = #2(bindVar (env, f))

    fun lookup (env, s) = (case E.find(env, x)
	   of SOME x' => x'
	    | NONE => raise Fail("unbound variable " ^ BV.toString x)
	  (* end case *))
    fun lookupVars (env, xs) = List.map (fn x => lookup(env, x)) xs

    fun cvtLambda (env, B.FB{f, params, exh, body}) = let
	  val f' = lookup(env, f)
	  val (params', env) = List.foldl
		(fn (v, (vs', env)) => let
		  val (v', env) = bindVar(env, v)
		  in (v'::vs', env) end
		) ([], env) params
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
	   of B.E_Let(xs, e1, e2) =>
	    | B.E_Stmt(xs, rhs, e) =>
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
	    | B.E_Case of (var * (pat * exp) list * exp option)
	    | B.E_Apply(f, params, exh) =
		C.Apply(lookup(env, f),
		  lookupVars(env, params),
		  [retK', lookupVar(env, exh)])
	    | B.E_Throw(k, xs) => C.Throw(lookup(env, k), lookupVars(env, xs))
	    | B.E_Ret xs => C.Throw(retK', lookupVars(env, xs))
	  (* end case *))

  (* convert an expression that is in a non-tail position *)
    and cvtE (env, B.E_Pt(_, e), k) = (case e
	   of B.E_Let(xs, e1, e2) =>
	    | B.E_Stmt(xs, rhs, e) =>
	    | B.E_Fun(fbs, e) =>
		cvtFun (env, fbs,
		  fn (env2, fbs') => C.mkFun(fbs', cvtE(env2, e, k)))
	    | B.E_Cont(kb, e) =>
		cvtCont (env, fb,
		  fn (env2, fb') => C.mkCont(fb', cvtE(env2, e, k)))
	    | B.E_If(x, e1, e2) => let
		val joink' = CV.new("joinK", ??)
		val a' = CV.nwe("a", ??)
		in
		  C.mkCont(
		    C.FB{f=joinK', params=[a'], rets=[], body=k [a']},
		    C.If(lookup(env, x),
		      cvtTailE(env, joink'),
		      cvtTailE(env, joink')))
		end
	    | B.E_Case of (var * (pat * exp) list * exp option)
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
