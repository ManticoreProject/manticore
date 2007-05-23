(* expand.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module converts the more compact parse-tree representation of CPS code
 * into the internal representation of CPS terms.
 *)

structure Expand =
  struct

    structure PT = CPSPT
    structure P = Prim
    structure Ty = CPSTy

  (* table mapping primop names to prim_info *)
    structure MkPrim = MakePrimFn (
	type var = CPS.var
	type ty = Ty.ty
	val anyTy = Ty.T_Any
	val boolTy = Ty.boolTy
	val addrTy = Ty.T_Addr(Ty.T_Any)
	val rawTy = Ty.T_Raw)

    datatype prim_info = datatype MkPrim.prim_info
    val findPrim = MkPrim.findPrim

  (* some type utilities *)
    fun unwrapType (Ty.T_Wrap rTy) = Ty.T_Raw rTy
      | unwrapType ty = raise Fail(concat["unwrapType(", Ty.toString ty, ")"])

    fun selectType (i, Ty.T_Tuple (_, tys)) = List.nth(tys, i)
      | selectType _ = raise Fail "selectType"

    fun lookup (env, x) = (case AtomMap.find(env, x)
	   of NONE => raise Fail("unbound variable " ^ Atom.toString x)
	    | SOME x' => x'
	  (* end case *))

    fun newTmp ty = CPS.Var.new("_t", ty)

    fun cvtVarBinds (env, vars) = let
	  fun f ((x, ty), (env, xs)) = let
		val x' = CPS.Var.new(Atom.toString x, ty)
		in
		  (AtomMap.insert(env, x, x'), x'::xs)
		end
	  in
	    List.foldl f (env, []) vars
	  end

    fun cvtExp (env, e) = (case e
	   of PT.Let(lhs, rhs, e) => let
		val (env', lhs') = cvtVarBinds (env, lhs)
		val lhs' = List.rev lhs'
		val e' = cvtExp(env', e)
		in
		  case rhs
		   of PT.SimpleExp e => (case e
			 of PT.Var x => CPS.mkLet(lhs', CPS.Var[lookup (env, x)], e')
			  | PT.Select(i, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				CPS.mkLet(lhs', CPS.Select(i, x), e'))
			  | PT.Cast(ty, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				CPS.mkLet(lhs', CPS.Cast(ty, x), e'))
			  | PT.Literal(lit, optTy) => let
			      val rhs = (case (lit, optTy)
				     of (Literal.Enum w, _) => CPS.Const(lit, Ty.T_Enum w)
				      | (Literal.Int _, SOME ty) => CPS.Const(lit, Ty.T_Wrap ty)
			              | (Literal.Int _, NONE) => CPS.Const(lit, Ty.T_Wrap Ty.T_Int)
				      | (Literal.Float _, SOME ty) => CPS.Const(lit, Ty.T_Wrap ty)
			              | (Literal.Float _, NONE) => CPS.Const(lit, Ty.T_Wrap Ty.T_Float)
			              | (Literal.String s, _) => CPS.Const(lit, Ty.T_Any)
				    (* end case *))
			      in
				CPS.mkLet(lhs', rhs, e')
			      end
			  | PT.Unwrap arg =>
			      cvtSimpleExp (env, arg, fn x =>
				CPS.mkLet(lhs', CPS.Unwrap x, e'))
			  | PT.Prim(p, args) =>
			      cvtSimpleExps (env, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
					| (SOME(Prim1{mk, ...}), [x]) => mk x
					| (SOME(Prim2{mk, ...}), [x, y]) => mk(x, y)
					| (SOME(Prim3{mk, ...}), [x, y, z]) => mk(x, y, z)
					| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
				      (* end case *))
				in
				  CPS.mkLet(lhs', CPS.Prim rhs, e')
				end)
			  | PT.HostVProc => CPS.mkLet(lhs', CPS.HostVProc, e')
			(* end case *))
		    | PT.Alloc args =>
			cvtSimpleExps (env, args, fn xs => CPS.mkLet(lhs', CPS.Alloc xs, e'))
		    | PT.Wrap arg =>
			cvtSimpleExp (env, arg, fn x => CPS.mkLet(lhs', CPS.Wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps (env, args, fn xs =>
			  CPS.mkLet(lhs', CPS.CCall(lookup(env, f), xs), e'))
		    | PT.Dequeue(vp) => cvtSimpleExp (env, vp, fn vp =>
			  CPS.mkLet(lhs', CPS.Dequeue vp, e'))
		    | PT.Enqueue(vp, tid, fiber) =>
			cvtSimpleExps (env, [vp, tid, fiber], fn [vp, tid, fiber] =>
			  CPS.mkLet(lhs', CPS.Enqueue(vp, tid, fiber), e'))
		  (* end case *)
		end
	    | PT.Fun(fbs, e) => let
		fun f (fb, (env', cvtBodies)) = let
			val (env'', cvt) = cvtLambda (env', fb)
			in
			  (env'', cvt::cvtBodies)
			end
		val (envWFBs, cvtBodies) = List.foldl f (env, []) fbs
		in
		  CPS.mkFun(
		    List.foldl (fn (cvt, fbs) => cvt envWFBs :: fbs) [] cvtBodies,
		    cvtExp (envWFBs, e))
		end
	    | PT.Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val (env', cvtBody) = cvtLambda(env, fb)
		in
		  CPS.mkCont(cvtBody env', cvtExp(env', e))
		end
	    | PT.If(e1, e2, e3) =>
		cvtSimpleExp (env, e1, fn x => CPS.If(x, cvtExp(env, e2), cvtExp(env, e3)))
	    | PT.Switch(arg, cases, dflt) => 
                cvtSimpleExp (env, arg, fn arg =>
                  CPS.Switch (
		    arg, 
                    List.map (fn (i,e) => (Word.fromInt i, cvtExp(env,e))) cases,
                    case dflt of NONE => NONE | SOME e => SOME (cvtExp(env, e))))
	    | PT.Apply(f, args, rets) =>
		cvtSimpleExps (env, args,
		  fn xs => cvtSimpleExps (env, rets, fn ys => CPS.Apply(lookup(env, f), xs, ys)))
	    | PT.Throw(k, args) =>
		cvtSimpleExps (env, args, fn xs => CPS.Throw(lookup(env, k), xs))
	    | PT.Run(vp, act, fiber) => CPS.Run{
		  vp = lookup(env, vp),
		  act = lookup (env, act),
		  fiber = lookup (env, fiber)
		}
	    | PT.Forward(vp, sign) => CPS.Forward{
		  vp = lookup(env, vp),
		  sign = lookup(env, sign)
		}
	  (* end case *))

    and cvtLambda (env, (f, params, rets, e)) = let
	  val fnTy = Ty.T_Fun(List.map #2 params, List.map #2 rets)
	  val f' = CPS.Var.new(Atom.toString f, fnTy)
	  fun doBody env = let
		val (envWParams, params') = cvtVarBinds (env, params)
		val (envWParams, rets') = cvtVarBinds (envWParams, rets)
		in
		  CPS.FB{
		      f = f', params = List.rev params',
		      rets = List.rev rets', body = cvtExp (envWParams, e)
		    }
		end
	  in
	    (AtomMap.insert(env, f, f'), doBody)
	  end

    and cvtSimpleExp (env, e, k : CPS.var -> CPS.exp) = (case e
	   of PT.Var x => k(lookup(env, x))
	    | PT.Select(i, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(selectType(i, CPS.Var.typeOf x))
		  in
		    CPS.mkLet([tmp], CPS.Select(i, x), k tmp)
		  end)
	    | PT.Literal (lit, optTy) => let
		  val ty = (case optTy
			 of NONE => Ty.T_Any
			  | SOME rTy => Ty.T_Raw rTy
			(* end case *))
		  val tmp = newTmp ty
		  in
		    CPS.mkLet([tmp], CPS.Const(lit, ty), k tmp)
		  end
	    | PT.Cast(ty, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp ty
		  in
		    CPS.mkLet([tmp], CPS.Cast(ty, x), k tmp)
		  end)
	    | PT.Unwrap e =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(unwrapType(CPS.Var.typeOf x))
		  in
		    CPS.mkLet([tmp], CPS.Unwrap x, k tmp)
		  end)
	    | PT.Prim(p, args) =>
		cvtSimpleExps (env, args, fn xs => let
		  val (lhs, rhs) = (case (findPrim p, xs)
			 of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
			  | (SOME(Prim1{mk, resTy, ...}), [x]) => (newTmp resTy, mk x)
			  | (SOME(Prim2{mk, resTy, ...}), [x, y]) => (newTmp resTy, mk(x, y))
			  | (SOME(Prim3{mk, resTy, ...}), [x, y, z]) => (newTmp resTy, mk(x, y, z))
			  | _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
			(* end case *))
		  in
		    CPS.mkLet([lhs], CPS.Prim rhs, k lhs)
		  end)
	    | PT.HostVProc => let
		val tmp = newTmp(Ty.T_VProc)
		in
		  CPS.mkLet([tmp], CPS.HostVProc, k tmp)
		end
	  (* end case *))

    and cvtSimpleExps (env, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (env, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

    fun cvtModule (PT.MODULE{name, externs, body}) = let
	  fun doCFun (CFunctions.CFun{var, name, retTy, argTys, attrs}, (cfs, env)) = let
		val f = CPS.Var.new(Atom.toString var, Ty.T_CFun(CFunctions.CProto(retTy, argTys, attrs)))
		in (
		  CPS.mkCFun{var=f, name=name, retTy=retTy, argTys=argTys, attrs=attrs}::cfs,
		  AtomMap.insert(env, var, f)
		) end
	  val (cfs, env) = List.foldl doCFun ([], AtomMap.empty) externs
	  val (_, cvtBody) = cvtLambda (AtomMap.empty, body)
	  in
	    CPS.MODULE{name=name, externs=cfs, body=cvtBody env}
	  end

  end
