(* expand.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module converts the more compact parse-tree representation of
 BOM code into the internal representation of BOM terms.
 *)
 
 structure Expand =
  struct

    structure PT = BOMPT
    structure P = Prim
    structure Ty = BOMTy
    structure BV = BOM.Var

  (* table mapping primop names to prim_info *)
    structure MkPrim = MakePrimFn (
	type var = BOM.var
	type ty = Ty.ty
	val anyTy = Ty.T_Any
	val boolTy = Ty.boolTy
	val rawTy = Ty.T_Raw)

    datatype prim_info = datatype MkPrim.prim_info
    val findPrim = MkPrim.findPrim

  (* some type utilities *)
    fun unwrapType (Ty.T_Wrap rTy) = Ty.T_Raw rTy
      | unwrapType ty = raise Fail(concat["unwrapType(", Ty.toString ty, ")"])

    fun selectType (i, Ty.T_Tuple(_, tys)) = List.nth(tys, i)
      | selectType _ = raise Fail "selectType"

    fun lookup (env, x) = (case AtomMap.find(env, x)
	   of NONE => raise Fail("unbound variable " ^ Atom.toString x)
	    | SOME x' => x'
	  (* end case *))

    fun newTmp ty = BOM.Var.new("_t", ty)

  (* convert a parse-tree type express to a BOM type *)
    fun cvtTy PT.T_Any = Ty.T_Any
      | cvtTy (PT.T_Enum w) = Ty.T_Enum w
      | cvtTy (PT.T_Raw rty) = Ty.T_Raw rty
      | cvtTy (PT.T_Wrap rty) = Ty.T_Wrap rty
      | cvtTy (PT.T_Tuple(mut, tys)) = Ty.T_Tuple(mut, List.map cvtTy tys)
      | cvtTy (PT.T_Addr ty) = Ty.T_Addr(cvtTy ty)
      | cvtTy (PT.T_Fun(argTys, exhTys, resTys)) =
	  Ty.T_Fun(List.map cvtTy argTys, List.map cvtTy exhTys, List.map cvtTy resTys)
      | cvtTy (PT.T_Cont tys) = Ty.T_Cont(List.map cvtTy tys)
      | cvtTy (PT.T_CFun cproto) = Ty.T_CFun cproto
      | cvtTy (PT.T_VProc) = Ty.T_VProc
      | cvtTy (PT.T_TyCon tyc) = (case Basis.findTyc tyc
	   of SOME tyc => Ty.T_TyCon tyc
	    | NONE => raise Fail(concat["unknown type constructor ", Atom.toString tyc])
	  (* end case *))

    fun cvtVarBinds (env, vars) = let
	  fun f ((x, ty), (env, xs)) = let
		val ty = cvtTy ty
		val x' = BOM.Var.new(Atom.toString x, ty)
		in
		  (AtomMap.insert(env, x, x'), x'::xs)
		end
	  in
	    List.foldl f (env, []) vars
	  end
 
    fun cvtPat (env, PT.DConPat(dc, xs)) = (case Basis.findDCon dc
	   of SOME dc => let
		fun f (PT.WildPat, (env, xs)) = let
		      val x' = BOM.Var.new("_wild", Ty.T_Any)
		      in
			(env, x'::xs)
		      end
		  | f (PT.VarPat(x, ty), (env, xs)) = let
		      val ty = cvtTy ty
		      val x' = BOM.Var.new(Atom.toString x, ty)
		      in
			(AtomMap.insert(env, x, x'), x'::xs)
		      end
		val (env, xs) = List.foldl f (env, []) xs
		in
		  (env, BOM.P_DCon(dc, List.rev xs))
		end
	    | NONE => raise Fail(concat["unknown data constructor ", Atom.toString dc])
	  (* end case *))
      | cvtPat (env, PT.ConstPat(const, ty)) = (env, BOM.P_Const(const, cvtTy ty))

    fun cvtExp (env, e) = (case e
	   of PT.Let(lhs, rhs, e) => let
		val (env', lhs') = cvtVarBinds (env, lhs)
		val lhs' = List.rev lhs'
		val e' = cvtExp(env', e)
		in
		  case rhs
		   of PT.Exp e => BOM.mkLet(lhs', cvtExp(env, e), e')
		    | PT.SimpleExp e => (case e
			 of PT.Var x => BOM.mkLet(lhs', BOM.mkRet[lookup(env, x)], e')
			  | PT.Select(i, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Select(i, x), e'))
			  | PT.AddrOf(i, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_AddrOf(i, x), e'))
			  | PT.Cast(ty, arg) =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Cast(cvtTy ty, x), e'))
	                  | PT.Const(lit, ty) => BOM.mkStmt(lhs', BOM.E_Const(lit, cvtTy ty), e')
			  | PT.Unwrap arg =>
			      cvtSimpleExp (env, arg, fn x =>
				BOM.mkStmt(lhs', BOM.E_Unwrap x, e'))
			  | PT.Prim(p, args) =>
			      cvtSimpleExps (env, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
					| (SOME(Prim1{mk, ...}), [x]) => mk x
					| (SOME(Prim2{mk, ...}), [x, y]) => mk(x, y)
					| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
				      (* end case *))
				in
				  BOM.mkStmt(lhs', BOM.E_Prim rhs, e')
				end)
			  | PT.HostVProc => BOM.mkStmt(lhs', BOM.E_HostVProc, e')
			(* end case *))
		    | PT.Update(i, arg, rhs) =>
			cvtSimpleExp (env, arg, fn x =>
			  cvtSimpleExp (env, rhs, fn y =>
			    BOM.mkStmt(lhs', BOM.E_Update(i, x, y), e')))
		    | PT.Alloc args => let
			val mut = (case BV.typeOf(hd lhs')
			       of Ty.T_Tuple(true, _) => true
				| _ => false
			      (* end case *))
			in
			  cvtSimpleExps (env, args,
			    fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(Ty.T_Tuple(mut, List.map BV.typeOf xs), xs),
				e'))
			end
		    | PT.Wrap arg =>
			cvtSimpleExp (env, arg, fn x => BOM.mkStmt(lhs', BOM.E_Wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps (env, args, fn xs =>
			  BOM.mkStmt(lhs', BOM.E_CCall(lookup(env, f), xs), e'))
		  (* end case *)
		end
	    | PT.Fun(fbs, e) => let
		fun f (fb, (env', cvtBodies)) = let
			val (env'', cvt) = cvtLambda (env', fb, Ty.T_Fun)
			in
			  (env'', cvt::cvtBodies)
			end
		val (envWFBs, cvtBodies) = List.foldl f (env, []) fbs
		in		 
		  BOM.mkFun(
		    List.foldl (fn (cvt, fbs) => cvt envWFBs :: fbs) [] cvtBodies,
		    cvtExp (envWFBs, e))
		end
	    | PT.Cont(fb, e) => let
	      (* NOTE: continuations are permitted to be recursive *)
		val (env', cvtBody) = cvtLambda(env, fb, fn (argTys, _, _) => Ty.T_Cont argTys)
		in
		  BOM.mkCont(cvtBody env', cvtExp(env', e))
		end
	    | PT.If(e1, e2, e3) =>
		cvtSimpleExp (env, e1, fn x => BOM.mkIf(x, cvtExp(env, e2), cvtExp(env, e3)))
	    | PT.Case(arg, cases, dflt) => let
		fun doCase (pat, exp) = let
		      val (env', pat') = cvtPat(env, pat)
		      in
			(pat', cvtExp(env', exp))
		      end
                in
		  cvtSimpleExp (env, arg, fn arg =>
                    BOM.mkCase(
		      arg, 
                      List.map doCase cases,
                      case dflt
		       of NONE => NONE
			| SOME(PT.WildPat, e) => SOME (cvtExp(env, e))
			| SOME(PT.VarPat(x, _), e) => SOME (cvtExp(AtomMap.insert(env, x, arg), e))
		      (* end case *)))
		end
	    | PT.Apply(f, args, rets) =>
		cvtSimpleExps (env, args,
		  fn xs => cvtSimpleExps (env, rets,
		    fn ys => BOM.mkApply(lookup(env, f), xs, ys)))
	    | PT.Throw(k, args) =>
		cvtSimpleExps (env, args, fn xs => BOM.mkThrow(lookup(env, k), xs))
	    | PT.Return args =>
		cvtSimpleExps (env, args, fn xs => BOM.mkRet xs)
	    | PT.HLOpApply(hlop, args, rets) => (case Basis.findHLOp hlop
		 of SOME hlop =>
		      cvtSimpleExps (env, args,
			fn xs => cvtSimpleExps (env, rets,
			  fn ys => BOM.mkHLOp(hlop, xs, ys)))
		  | NONE => raise Fail(concat["unknown high-level op ", Atom.toString hlop])
		(* end case *))
	  (* end case *))

    and cvtLambda (env, (f, params, rets, tys, e), tyCon) = let
	  fun cvt (_, ty) = cvtTy ty
	  val fnTy = tyCon(List.map cvt params, List.map cvt rets, List.map cvtTy tys)
	  val f' = BOM.Var.new(Atom.toString f, fnTy)
	  fun doBody env = let
		val (envWParams, params') = cvtVarBinds (env, params)
		val (envWParams, rets') = cvtVarBinds (envWParams, rets)
		in
		  BOM.FB{
		      f = f', params = List.rev params',
		      exh = List.rev rets', body = cvtExp (envWParams, e)
		    }
		end
	  in
	    (AtomMap.insert(env, f, f'), doBody)
	  end

    and cvtSimpleExp (env, e, k : BOM.var -> BOM.exp) = (case e
	   of PT.Var x => k(lookup(env, x))
	    | PT.Select(i, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(selectType(i, BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.E_Select(i, x), k tmp)
		  end)
	    | PT.AddrOf(i, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(Ty.T_Addr(selectType(i, BOM.Var.typeOf x)))
		  in
		    BOM.mkStmt([tmp], BOM.E_AddrOf(i, x), k tmp)
		  end)
	    | PT.Const(lit, ty) => let
		  val ty = cvtTy ty
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Const(lit, ty), k tmp)
		  end
	    | PT.Cast(ty, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val ty = cvtTy ty
		  val tmp = newTmp ty
		  in
		    BOM.mkStmt([tmp], BOM.E_Cast(ty, x), k tmp)
		  end)
	    | PT.Unwrap e =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(unwrapType(BOM.Var.typeOf x))
		  in
		    BOM.mkStmt([tmp], BOM.E_Unwrap x, k tmp)
		  end)
	    | PT.Prim(p, args) => let
		fun mkBind xs = (case (findPrim p, xs)
		       of (NONE, _) => (case Basis.findDCon p
			     of NONE => raise Fail("unknown primop " ^ Atom.toString p)
			      | SOME dc =>
				  (newTmp(BOMTyCon.dconResTy dc), BOM.E_DCon(dc, xs))
			    (* end case *))
			| (SOME(Prim1{mk, resTy, ...}), [x]) =>
			    (newTmp resTy, BOM.E_Prim(mk x))
			| (SOME(Prim2{mk, resTy, ...}), [x, y]) =>
			    (newTmp resTy, BOM.E_Prim(mk(x, y)))
			| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
		      (* end case *))
		in
		  cvtSimpleExps (env, args, fn xs => let
		    val (lhs, rhs) = mkBind xs
		    in
		      BOM.mkStmt([lhs], rhs, k lhs)
		    end)
		end
	    | PT.HostVProc =>  let
		  val tmp = newTmp Ty.T_VProc
		  in
		    BOM.mkStmt([tmp], BOM.E_HostVProc, k tmp)
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
		val f = BOM.Var.new(Atom.toString var, Ty.T_CFun(CFunctions.CProto(retTy, argTys, attrs)))
		in (
		  BOM.mkCFun{var=f, name=name, retTy=retTy, argTys=argTys, attrs=attrs}::cfs,
		  AtomMap.insert(env, var, f)
		) end
	  val (cfs, env) = List.foldl doCFun ([], AtomMap.empty) externs
	  val (_, cvtBody) = cvtLambda (AtomMap.empty, body, Ty.T_Fun)
	  in
	    BOM.MODULE{name=name, externs=cfs, body=cvtBody env}
	  end

  end
