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

    datatype prim_info
      = Prim1 of {
	    mk : BOM.var -> BOM.var P.prim,
	    argTy : Ty.ty,
	    resTy : Ty.ty
	  }
      | Prim2 of {
	    mk : BOM.var * BOM.var -> BOM.var P.prim,
	    argTy : Ty.ty * Ty.ty,
	    resTy : Ty.ty
	  }
	  
  (* table mapping primop names to prim_info *)
    val findPrim = let
	  val tbl = AtomTable.mkTable(128, Fail "prim table")
	  val ins = AtomTable.insert tbl
	  val aTy = Ty.T_Any
	  val bTy = Ty.boolTy
	  val i32 = Ty.T_Raw Ty.T_Int
	  val i64 = Ty.T_Raw Ty.T_Long
	  val f32 = Ty.T_Raw Ty.T_Float
	  val f64 = Ty.T_Raw Ty.T_Double
	  fun mk cons (mk, argTy, resTy) = cons {mk=mk, argTy=argTy, resTy  = resTy}
	  in
	    List.app (fn (n, info) => ins(Atom.atom n, info)) [
		("isBoxed",	mk Prim1 (P.isBoxed,	aTy,		bTy)),
		("isUnboxed",	mk Prim1 (P.isUnboxed,	aTy,		bTy)),
		("Equal",	mk Prim2 (P.Equal,	(aTy, aTy),	bTy)),
		("NotEqual",	mk Prim2 (P.NotEqual,	(aTy, aTy),	bTy)),
		("BNot",	mk Prim1 (P.BNot,	bTy,		bTy)),
		("BEq",		mk Prim2 (P.BEq,	(bTy, bTy),	bTy)),
		("BNEq",	mk Prim2 (P.BNEq,	(bTy, bTy),	bTy)),
		("I32Add",	mk Prim2 (P.I32Add,	(i32, i32),	i32)),
		("I32Sub",	mk Prim2 (P.I32Sub,	(i32, i32),	i32)),
		("I32Mul",	mk Prim2 (P.I32Mul,	(i32, i32),	i32)),
		("I32Div",	mk Prim2 (P.I32Div,	(i32, i32),	i32)),
		("I32Mod",	mk Prim2 (P.I32Mod,	(i32, i32),	i32)),
		("I32Neg",	mk Prim1 (P.I32Neg,	i32,		i32)),
		("I32Eq",	mk Prim2 (P.I32Eq,	(i32, i32),	bTy)),
		("I32NEq",	mk Prim2 (P.I32NEq,	(i32, i32),	bTy)),
		("I32Lt",	mk Prim2 (P.I32Lt,	(i32, i32),	bTy)),
		("I32Lte",	mk Prim2 (P.I32Lte,	(i32, i32),	bTy)),
		("I32Gt",	mk Prim2 (P.I32Gt,	(i32, i32),	bTy)),
		("I32Gte",	mk Prim2 (P.I32Gte,	(i32, i32),	bTy)),
		("I64Add",	mk Prim2 (P.I64Add,	(i64, i64),	i64)),
		("I64Sub",	mk Prim2 (P.I64Sub,	(i64, i64),	i64)),
		("I64Mul",	mk Prim2 (P.I64Mul,	(i64, i64),	i64)),
		("I64Div",	mk Prim2 (P.I64Div,	(i64, i64),	i64)),
		("I64Mod",	mk Prim2 (P.I64Mod,	(i64, i64),	i64)),
		("I64Neg",	mk Prim1 (P.I64Neg,	i64,		i64)),
		("I64Eq",	mk Prim2 (P.I64Eq,	(i64, i64),	bTy)),
		("I64NEq",	mk Prim2 (P.I64NEq,	(i64, i64),	bTy)),
		("I64Lt",	mk Prim2 (P.I64Lt,	(i64, i64),	bTy)),
		("I64Lte",	mk Prim2 (P.I64Lte,	(i64, i64),	bTy)),
		("I64Gt",	mk Prim2 (P.I64Gt,	(i64, i64),	bTy)),
		("I64Gte",	mk Prim2 (P.I64Gte,	(i64, i64),	bTy)),
		("F32Add",	mk Prim2 (P.F32Add,	(f32, f32),	f32)),
		("F32Sub",	mk Prim2 (P.F32Sub,	(f32, f32),	f32)),
		("F32Mul",	mk Prim2 (P.F32Mul,	(f32, f32),	f32)),
		("F32Div",	mk Prim2 (P.F32Div,	(f32, f32),	f32)),
		("F32Neg",	mk Prim1 (P.F32Neg,	f32,		f32)),
		("F32Eq",	mk Prim2 (P.F32Eq,	(f32, f32),	bTy)),
		("F32NEq",	mk Prim2 (P.F32NEq,	(f32, f32),	bTy)),
		("F32Lt",	mk Prim2 (P.F32Lt,	(f32, f32),	bTy)),
		("F32Lte",	mk Prim2 (P.F32Lte,	(f32, f32),	bTy)),
		("F32Gt",	mk Prim2 (P.F32Gt,	(f32, f32),	bTy)),
		("F32Gte",	mk Prim2 (P.F32Gte,	(f32, f32),	bTy)),
		("F64Add",	mk Prim2 (P.F64Add,	(f64, f64),	f64)),
		("F64Sub",	mk Prim2 (P.F64Sub,	(f64, f64),	f64)),
		("F64Mul",	mk Prim2 (P.F64Mul,	(f64, f64),	f64)),
		("F64Div",	mk Prim2 (P.F64Div,	(f64, f64),	f64)),
		("F64Neg",	mk Prim1 (P.F64Neg,	f64,		f64)),
		("F64Eq",	mk Prim2 (P.F64Eq,	(f64, f64),	bTy)),
		("F64NEq",	mk Prim2 (P.F64NEq,	(f64, f64),	bTy)),
		("F64Lt",	mk Prim2 (P.F64Lt,	(f64, f64),	bTy)),
		("F64Lte",	mk Prim2 (P.F64Lte,	(f64, f64),	bTy)),
		("F64Gt",	mk Prim2 (P.F64Gt,	(f64, f64),	bTy)),
		("F64Gte",	mk Prim2 (P.F64Gte,	(f64, f64),	bTy))
	      ];
	    AtomTable.find tbl
	  end

  (* some type utilities *)
    fun unwrapType (Ty.T_Wrap rTy) = Ty.T_Raw rTy
      | unwrapType ty = raise Fail(concat["unwrapType(", Ty.toString ty, ")"])

    fun selectType (i, Ty.T_Tuple tys) = List.nth(tys, i)
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
      | cvtTy (PT.T_Tuple tys) = Ty.T_Tuple(List.map cvtTy tys)
      | cvtTy (PT.T_Fun(argTys, exhTys, resTys)) =
	  Ty.T_Fun(List.map cvtTy argTys, List.map cvtTy exhTys, List.map cvtTy resTys)
      | cvtTy (PT.T_Cont tys) = Ty.T_Cont(List.map cvtTy tys)
      | cvtTy (PT.T_CFun cproto) = Ty.T_CFun cproto
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
			(* end case *))
		    | PT.Alloc args =>
			cvtSimpleExps (env, args,
			  fn xs => BOM.mkStmt(lhs', BOM.E_Alloc(Ty.T_Tuple(List.map BV.typeOf xs), xs), e'))
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
