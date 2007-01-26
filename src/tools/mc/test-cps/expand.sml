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

    structure P = Prim
    structure Ty = CPSTy

    datatype prim_info
      = Prim1 of {
	    mk : CPS.var -> CPS.var P.prim,
	    argTy : Ty.ty,
	    resTy : Ty.ty
	  }
      | Prim2 of {
	    mk : CPS.var -> CPS.var P.prim,
	    argTy : Ty.ty * Ty.ty,
	    resTy : Ty.ty
	  }

  (* table mapping primop names to prim_info *)
    val findPrim = let
	  val tbl = AtomTable.mkTable(128, raise Fail "prim table")
	  val ins = AtomTable.insert tbl
	  val bTy = Ty.T_Bool
	  val i32 = Ty.T_Raw Ty.T_Int
	  val i64 = Ty.T_Raw Ty.T_Long
	  in
	    List.app (fn (n, info) => ins(Atom.atom n, info)) [
		("BNot",	Prim1(P.BNot,	bTy,		bTy)),
		("BEq",		Prim2(P.BEq,	(bTy, bTy),	bTy)),
		("BNEq",	Prim2(P.BNEq,	(bTy, bTy),	bTy)),
		("I32Add",	Prim2(P.I32Add,	(i32, i32),	i32)),
		("I32Sub",	Prim2(P.I32Sub,	(i32, i32),	i32)),
		("I32Mul",	Prim2(P.I32Mul,	(i32, i32),	i32)),
		("I32Div",	Prim2(P.I32Div,	(i32, i32),	i32)),
		("I32Mod",	Prim2(P.I32Mod,	(i32, i32),	i32)),
		("I32Neg",	Prim1(P.I32Neg,	i32,		i32)),
		("I32Eq",	Prim2(P.I32Eq,	(i32, i32),	bTy)),
		("I32NEq",	Prim2(P.I32NEq,	(i32, i32),	bTy)),
		("I32Lt",	Prim2(P.I32Lt,	(i32, i32),	bTy)),
		("I32Lte",	Prim2(P.I32Lte,	(i32, i32),	bTy)),
		("I32Gt",	Prim2(P.I32Gt,	(i32, i32),	bTy)),
		("I32Gte",	Prim2(P.I32Gte,	(i32, i32),	bTy)),
		("I64Add",	Prim2(P.I64Add,	(i64, i64),	i64)),
		("I64Sub",	Prim2(P.I64Sub,	(i64, i64),	i64)),
		("I64Mul",	Prim2(P.I64Mul,	(i64, i64),	i64)),
		("I64Div",	Prim2(P.I64Div,	(i64, i64),	i64)),
		("I64Mod",	Prim2(P.I64Mod,	(i64, i64),	i64)),
		("I64Neg",	Prim1(P.I64Neg,	i64,		i64)),
		("I64Eq",	Prim2(P.I64Eq,	(i64, i64),	bTy)),
		("I64NEq",	Prim2(P.I64NEq,	(i64, i64),	bTy)),
		("I64Lt",	Prim2(P.I64Lt,	(i64, i64),	bTy)),
		("I64Lte",	Prim2(P.I64Lte,	(i64, i64),	bTy)),
		("I64Gt",	Prim2(P.I64Gt,	(i64, i64),	bTy)),
		("I64Gte",	Prim2(P.I64Gte,	(i64, i64),	bTy)),
		("F32Add",	Prim2(P.F32Add,	(f32, f32),	f32)),
		("F32Sub",	Prim2(P.F32Sub,	(f32, f32),	f32)),
		("F32Mul",	Prim2(P.F32Mul,	(f32, f32),	f32)),
		("F32Div",	Prim2(P.F32Div,	(f32, f32),	f32)),
		("F32Neg",	Prim1(P.F32Neg,	f32,		f32)),
		("F32Eq",	Prim2(P.F32Eq,	(f32, f32),	bTy)),
		("F32NEq",	Prim2(P.F32NEq,	(f32, f32),	bTy)),
		("F32Lt",	Prim2(P.F32Lt,	(f32, f32),	bTy)),
		("F32Lte",	Prim2(P.F32Lte,	(f32, f32),	bTy)),
		("F32Gt",	Prim2(P.F32Gt,	(f32, f32),	bTy)),
		("F32Gte",	Prim2(P.F32Gte,	(f32, f32),	bTy)),
		("F64Add",	Prim2(P.F64Add,	(f64, f64),	f64)),
		("F64Sub",	Prim2(P.F64Sub,	(f64, f64),	f64)),
		("F64Mul",	Prim2(P.F64Mul,	(f64, f64),	f64)),
		("F64Div",	Prim2(P.F64Div,	(f64, f64),	f64)),
		("F64Neg",	Prim1(P.F64Neg,	f64,		f64)),
		("F64Eq",	Prim2(P.F64Eq,	(f64, f64),	bTy)),
		("F64NEq",	Prim2(P.F64NEq,	(f64, f64),	bTy)),
		("F64Lt",	Prim2(P.F64Lt,	(f64, f64),	bTy)),
		("F64Lte",	Prim2(P.F64Lte,	(f64, f64),	bTy)),
		("F64Gt",	Prim2(P.F64Gt,	(f64, f64),	bTy)),
		("F64Gte",	Prim2(P.F64Gte,	(f64, f64),	bTy))
	      ];
	    AtomTable.find tbl
	  end

    fun lookup (env, x) = (case AtomMap.find(enx, x)
	   of NONE => raise Fail("unbound variable " ^ Atom.toString x)
	    | SOME x' => x'
	  (* end case *))

    fun newTmp ty = CPS.Var.new(Atom.atom "_t", CPS.VK_None, ty)

    fun cvtVarBind kind ((x, ty), (env, xs)) = let
	  val x' = CPS.Var.new(x, kind, ty)
	  in
	    (AtomMap.insert(env, x, x'), x'::xs)
	  end

    fun cvtExp (env, e) = (case e
	   of PT.Let(lhs, rhs, e) => let
		val (env', lhs') = List.foldl cvtVarBind env lhs
		val e' = cvtExp(env', e)
		in
		  case rhs
		   of PT.SimpleExp e =>
			cvtSimpleExp (env, e, fn x => ??)
		    | PT.Alloc args =>
			cvtSimpleExps (env, args, fn xs => CPS.mkLet(lhs', CPS.Alloc xs, e'))
		    | PT.Wrap e =>
			cvtSimpleExp (env, e, fn x => CPS.mkLet(lhs', CPS.Wrap x, e'))
		    | PT.CCall(f, args) =>
			cvtSimpleExps (env, args, fn xs =>
			  CPS.mkLet(lhs', CPS.CCall(lookup f, xs), e'))
		  (* end case *)
		end
	    | PT.Fun(fbs, e) => ??
	    | PT.Cont(fb, e) => ??
	    | PT.If(e1, e2, e3) =>
		cvtSimpleExp (env, e1, fn x => CPS.If(x, cvtExp(env, e2), cvtExp(env, e3)))
	    | PT.Switch(arg, cases, dflt) => raise Fail "switch not implemented"
	    | PT.Apply(f, args) =>
		cvtSimpleExps (env, args, fn xs => CPS.Apply(lookup(env, x), xs))
	    | PT.Throw(k, args) =>
		cvtSimpleExps (env, args, fn xs => CPS.Throw(lookup(env, k), xs))
	  (* end case *))

    and cvtSimpleExp (env, e, k : CPS.var -> CPS.exp) = (case e
	   of PT.Var x => k(lookup x)
	    | PT.Select(i, e) =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(selectType(CPS.Var.typeOf x))
		  in
		    CPS.mkLet([tmp], CPS.Select(i, x), k tmp)
		  end
	    | PT.Literal lit =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp Ty.T_Any	(* FIXME *)
		  in
		    CPS.mkLet([tmp], CPS.Literal lit, k tmp)
		  end
	    | PT.Unwrap e =>
		cvtSimpleExp (env, e, fn x => let
		  val tmp = newTmp(unwrapType(CPS.Var.typeOf x))
		  in
		    CPS.mkLet([tmp], CPS.Unwrap x, k tmp)
		  end
	    | PT.Prim(p, args) =>
		cvtSimpleExps (env, args, fn xs => let
		  val (lhs, rhs) = (case (findPrim p, xs)
			 of (NONE, _) => raise Fail("unknown primop " ^ Atom.toString p)
			  | (SOME(Prim1(mkP, _, resTy), [x])) => (newTmp resTy, mkP x)
			  | (SOME(Prim2(mkP, _, resTy), [x, y])) => (newTmp resTy, mkP(x, y))
			  | _ => raise Fail("arity mismatch for primop " ^ Atom.toString p)
			(* end case *))
		  in
		    CPS.mkLet([lhs], CPS.Prim rhs, k lhs)
		  end)
	  (* end case *))

    and cvtSimpleExps (env, exps, k) = ??

  end
