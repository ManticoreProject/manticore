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

    datatype prim_info
      = Prim1 of {
	    mk : CPS.var -> CPS.var P.prim,
	    argTy : Ty.ty,
	    resTy : Ty.ty
	  }
      | Prim2 of {
	    mk : CPS.var * CPS.var -> CPS.var P.prim,
	    argTy : Ty.ty * Ty.ty,
	    resTy : Ty.ty
	  }

  (* table mapping primop names to prim_info *)
    val findPrim = let
	  val tbl = AtomTable.mkTable(128, Fail "prim table")
	  val ins = AtomTable.insert tbl
	  val bTy = Ty.boolTy
	  val i32 = Ty.T_Raw Ty.T_Int
	  val i64 = Ty.T_Raw Ty.T_Long
	  val f32 = Ty.T_Raw Ty.T_Float
	  val f64 = Ty.T_Raw Ty.T_Double
	  fun mk cons (mk, argTy, resTy) = cons {mk=mk, argTy=argTy, resTy  = resTy}
	  in
	    List.app (fn (n, info) => ins(Atom.atom n, info)) [
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

    fun newTmp ty = CPS.Var.new(Atom.atom "_t", ty)

    fun cvtVarBinds (env, vars) = let
	  fun f ((x, ty), (env, xs)) = let
		val x' = CPS.Var.new(x, ty)
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
			  | PT.Literal(lit, _) => CPS.mkLet(lhs', CPS.Literal lit, e')
			  | PT.Unwrap arg =>
			      cvtSimpleExp (env, arg, fn x =>
				CPS.mkLet(lhs', CPS.Unwrap x, e'))
			  | PT.Prim(p, args) =>
			      cvtSimpleExps (env, args, fn xs => let
				val rhs = (case (findPrim p, xs)
				       of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
					| (SOME(Prim1{mk, ...}), [x]) => mk x
					| (SOME(Prim2{mk, ...}), [x, y]) => mk(x, y)
					| _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
				      (* end case *))
				in
				  CPS.mkLet(lhs', CPS.Prim rhs, e')
				end)
			(* end case *))
		    | PT.Alloc args =>
			cvtSimpleExps (env, args, fn xs => CPS.mkLet(lhs', CPS.Alloc xs, e'))
		    | PT.Wrap arg =>
			cvtSimpleExp (env, arg, fn x => CPS.mkLet(lhs', CPS.Wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps (env, args, fn xs =>
			  CPS.mkLet(lhs', CPS.CCall(lookup(env, f), xs), e'))
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
		  CPS.mkFun(
		    List.foldl (fn (cvt, fbs) => cvt envWFBs :: fbs) [] cvtBodies,
		    cvtExp (envWFBs, e))
		end
	    | PT.Cont(fb, e) => let
		val (env', cvtBody) = cvtLambda(env, fb, Ty.T_Cont)
		in
		  CPS.mkCont(cvtBody env, cvtExp(env', e))
		end
	    | PT.If(e1, e2, e3) =>
		cvtSimpleExp (env, e1, fn x => CPS.If(x, cvtExp(env, e2), cvtExp(env, e3)))
	    | PT.Switch(arg, cases, dflt) => 
                cvtSimpleExp (env, arg, fn arg =>
                  CPS.Switch (
		    arg, 
                    List.map (fn (i,e) => (i, cvtExp(env,e))) cases,
                    case dflt of NONE => NONE | SOME e => SOME (cvtExp(env, e))))
	    | PT.Apply(f, args) =>
		cvtSimpleExps (env, args, fn xs => CPS.Apply(lookup(env, f), xs))
	    | PT.Throw(k, args) =>
		cvtSimpleExps (env, args, fn xs => CPS.Throw(lookup(env, k), xs))
	    | PT.Run(act, fiber) => CPS.Run{
		  act = lookup (env, act),
		  fiber = lookup (env, fiber)
		}
	    | PT.Forward sign => CPS.Forward(lookup(env, sign))
	  (* end case *))

    and cvtLambda (env, (f, params, e), tyCon) = let
	  val fnTy = tyCon(List.map #2 params)
	  val f' = CPS.Var.new(f, fnTy)
	  fun doBody env = let
		val (envWParams, params') = cvtVarBinds (env, params)
		in
		  (f', List.rev params', cvtExp (envWParams, e))
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
		    CPS.mkLet([tmp], CPS.Literal lit, k tmp)
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
			  | _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
			(* end case *))
		  in
		    CPS.mkLet([lhs], CPS.Prim rhs, k lhs)
		  end)
	  (* end case *))

    and cvtSimpleExps (env, exps, k) = let
	  fun cvt ([], tmps) = k(List.rev tmps)
	    | cvt (e::es, tmps) = cvtSimpleExp (env, e, fn t => cvt(es, t::tmps))
	  in
	    cvt (exps, [])
	  end

    fun cvtModule (PT.MODULE(cfuns, lambda)) = let
	  val (_, cvtBody) = cvtLambda (AtomMap.empty, lambda, Ty.T_Fun)
	  in
(* FIXME: need to do something with the C functions *)
	    CPS.MODULE(cvtBody AtomMap.empty)
	  end

  end
