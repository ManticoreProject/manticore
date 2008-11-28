(* convert.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Convert the BOM representation to CPS.  We assume that HL operator expansion
 * and case simplification have been performed.
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
    structure CTyUtil = CPSTyUtil
    structure E = BV.Map

  (* convert a BOM type to a CPS type *)
    fun cvtTy BTy.T_Any = CTy.T_Any
      | cvtTy (BTy.T_Enum w) = CTy.T_Enum w
      | cvtTy (BTy.T_Raw rTy) = CTy.T_Raw rTy
      | cvtTy (BTy.T_Tuple(mut, tys)) = CTy.T_Tuple(mut, List.map cvtTy tys)
      | cvtTy (BTy.T_Addr ty) = CTy.T_Addr(cvtTy ty)
      | cvtTy (BTy.T_Fun(paramTys, exhTys, retTys)) = let
	  val retKTy = CTy.contTy(List.map cvtTy retTys)
	  in
	    CTy.T_Fun(List.map cvtTy paramTys, retKTy :: List.map cvtTy exhTys)
	  end
      | cvtTy (BTy.T_Cont tys) = CTy.contTy(List.map cvtTy tys)
      | cvtTy (BTy.T_CFun cproto) = CTy.T_CFun cproto
      | cvtTy (BTy.T_VProc) = CTy.T_VProc
      | cvtTy (BTy.T_TyCon tyc) = raise Fail("unexpected tycon " ^ BOMTyCon.tycName tyc)

  (* create a new CPS variable using the name of a BOM variable *)
    fun newVar (v, ty') = CV.new(BV.nameOf v, ty')

  (* create a new CPS variable using the name and type of a BOM variable *)
    fun cvtVar v = newVar (v, cvtTy(BV.typeOf v))

    fun bindVar (env, v) = let
	  val v' = cvtVar v
	  in
	    (v', E.insert(env, v, v'))
	  end

    fun bindVars (env, vs) = let
	  fun bind (env, [], vs') = (List.rev vs', env)
	    | bind (env, v::vs, vs') = let
		val (v', env) = bindVar(env, v)
		in
		  bind(env, vs, v'::vs')
		end
	  in
	    bind (env, vs, [])
	  end

    fun bindLambda (B.FB{f, ...}, env) = #2(bindVar (env, f))

    fun lookup (env, x) = (case E.find(env, x)
	   of SOME x' => x'
	    | NONE => raise Fail("unbound variable " ^ BV.toString x)
	  (* end case *))
    fun lookupVars (env, xs) = List.map (fn x => lookup(env, x)) xs

  (* CPS convert a lambda; note that we assume that the function name has already been converted *)
    fun cvtLambda (env, B.FB{f, params, exh, body}) = let
	  val f' = lookup(env, f)
	  val (params', env) = bindVars (env, params)
	  val retK' = CV.new("retK", CTyUtil.returnTy(CV.typeOf f'))
	  val (exhs', env) = bindVars (env, exh)
	  val body' = cvtTailE(env, body, retK')
	  in
	    C.FB{f = f', params = params', rets = retK' :: exhs', body = body'}
	  end

  (* convert an expression that is in a tail position *)
    and cvtTailE (env, B.E_Pt(_, e), retK') = (case e
	   of B.E_Let(xs, e1, e2) => let
		val (xs', env2) = bindVars (env, xs)
		val tys' = List.map CV.typeOf xs'
		val joinK' = CV.new("letJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=xs', rets=[], body=cvtTailE(env2, e2, retK')},
		    cvtTailE (env, e1, joinK'))
		end
	    | B.E_Stmt(xs, rhs, e) =>
		cvtRHS (env, xs, rhs, fn env => cvtTailE(env, e, retK'))
	    | B.E_Fun(fbs, e) =>
		cvtFun (env, fbs,
		  fn (env2, fbs') => C.mkFun(fbs', cvtTailE(env2, e, retK')))
	    | B.E_Cont(fb, e) => let
		val (env, fb) = cvtCont(env, fb, retK')
		in
		  C.mkCont(fb, cvtTailE(env, e, retK'))
		end
	    | B.E_If(x, e1, e2) =>
		C.If(lookup(env, x),
		  cvtTailE (env, e1, retK'),
		  cvtTailE (env, e2, retK'))
	    | B.E_Case(x, cases, optDflt) => cvtCase (env, x, cases, optDflt, retK')
	    | B.E_Apply(f, args, exhs) =>
		C.Apply(lookup(env, f),
		  lookupVars(env, args),
		  retK' :: lookupVars(env, exhs))
	    | B.E_Throw(k, xs) => C.Throw(lookup(env, k), lookupVars(env, xs))
	    | B.E_Ret xs => C.Throw(retK', lookupVars(env, xs))
	    | B.E_HLOp(hlop, _, _) => raise Fail("unexpected high-level op " ^ HLOp.toString hlop)
	  (* end case *))

  (* convert an expression that is in a non-tail position; the tys' parameter is
   * a list of the result types returned by the expression.
   *)
    and cvtE (env, B.E_Pt(_, e), tys', k) = (case e
	   of B.E_Let(xs, e1, e2) => let
		val (xs', env2) = bindVars (env, xs)
		val tys2' = List.map CV.typeOf xs'
		val joinK' = CV.new("letJoinK", CTy.contTy tys2')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=xs', rets=[], body=cvtE(env2, e2, tys', k)},
		    cvtTailE (env, e1, joinK'))
		end
	    | B.E_Stmt(xs, rhs, e) =>
		cvtRHS (env, xs, rhs, fn env => cvtE(env, e, tys', k))
	    | B.E_Fun(fbs, e) =>
		cvtFun (env, fbs,
		  fn (env2, fbs') => C.mkFun(fbs', cvtE(env2, e, tys', k)))
	    | B.E_Cont(fb, e) => let
		val ys' = List.map (fn ty => CV.new("a", ty)) tys'
		val joinK' = CV.new("contJoinK", CTy.contTy tys')
		val (env, fb) = cvtCont(env, fb, joinK')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=ys', rets=[], body=k ys'},
		    C.mkCont(fb, cvtTailE(env, e, joinK')))
		end
	    | B.E_If(x, e1, e2) => let
		val ys' = List.map (fn ty => CV.new("a", ty)) tys'
		val joinK' = CV.new("ifJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=ys', rets=[], body=k ys'},
		    C.If(lookup(env, x),
		      cvtTailE(env, e1, joinK'),
		      cvtTailE(env, e2, joinK')))
		end
	    | B.E_Case(x, cases, optDflt) => let
		val ys' = List.map (fn ty => CV.new("a", ty)) tys'
		val joinK' = CV.new("caseJoinK", CTy.contTy tys')
		in
		  C.mkCont(
		    C.FB{f=joinK', params=ys', rets=[], body=k ys'},
		    cvtCase (env, x, cases, optDflt, joinK'))
		end
	    | B.E_Apply(f, params, exh) => let
		val f' = lookup(env, f)
		val params' = lookupVars(env, params)
		val exh' = lookupVars(env, exh)
		val retK' = CV.new("retK", CTyUtil.returnTy(CV.typeOf f'))
		val ys' = List.map (fn ty => CV.new("a", ty)) tys'
		in
		  C.mkCont(
		    C.FB{f=retK', params=ys', rets=[], body=k ys'},
		    C.Apply(f', params', retK' :: exh'))
		end
	    | B.E_Throw(k, xs) => C.Throw(lookup(env, k), lookupVars(env, xs))
	    | B.E_Ret xs => k(lookupVars(env, xs))
	    | B.E_HLOp _ => raise Fail "unexpected high-level op"
	  (* end case *))

    and cvtRHS (env, lhs, rhs, k) = let
	  fun cv x = lookup(env, x)
	  val (lhs', env2) = bindVars (env, lhs)
	  val rhs' = (case rhs
		 of B.E_Const(lit, ty) => C.Const(lit, cvtTy ty)
		  | B.E_Cast(ty, x) => C.Cast(cvtTy ty, cv x)
		  | B.E_Select(i, x) => C.Select(i, cv x)
		  | B.E_Update(i, x, z) => C.Update(i, cv x, cv z)
		  | B.E_AddrOf(i, x) => C.AddrOf(i, cv x)
		  | B.E_Alloc(ty, args) => C.Alloc(cvtTy ty, List.map cv args)
		  | B.E_Promote y => C.Promote(cv y)
		  | B.E_Prim p => C.Prim(PrimUtil.map cv p)
		  | B.E_DCon _ => raise Fail "unexpected DCon"
		  | B.E_CCall(f, args) => C.CCall(cv f, List.map cv args)
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
    and cvtCase (env, x, cases, dflt, retK') = let
	  fun cvtCase (B.P_Const(Literal.Enum tag, _), e) = (tag, cvtTailE(env, e, retK'))
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

    and cvtCont (env, fb as B.FB{f, params, exh=[], body}, k) = let
	  val env = bindLambda(fb, env)
	  val f' = lookup(env, f)
	  val (params', env) = bindVars (env, params)
	  val body' = cvtTailE(env, body, k)
	  in
	    (env, C.FB{f = f', params = params', rets = [], body = body'})
	  end

    fun transform (B.MODULE{name, externs, hlops, body}) = let
	  fun cvtExtern (CFunctions.CFun{var, name, retTy, argTys, attrs, varArg}, (cfs, env)) = let
		val (var', env) = bindVar(env, var)
		in
		  (CPS.mkCFun{var=var', name=name, retTy=retTy, argTys=argTys, attrs=attrs, varArg=varArg}::cfs, env)
		end
	  val (externs', env) = List.foldr cvtExtern ([], E.empty) externs
	  val env = bindLambda (body, env)
	  val C.Fun([body'],_) = C.mkFun([cvtLambda(env,body)],
					 C.Throw(C.Var.new("?",CTy.T_Any),[]))
	  in
	    C.MODULE{
		name = name, 
		externs = externs',
		body = body'
	      }
	  end

    val transform =
       BasicControl.mkKeepPass
       {preOutput = PrintBOM.output,
        preExt = "bom",
        postOutput = PrintCPS.output,
        postExt = "cps",
        passName = "convert",
        pass = transform,
        registry = ConvertControls.registry}

  end
