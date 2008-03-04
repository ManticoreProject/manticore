(* prim-ty-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Determine the return type of primitive operators.
 *)

functor PrimTyFn (Ty : sig

    structure V : VAR
    val bool    : V.ty
    val raw     : RawTypes.raw_ty -> V.ty

  end) : sig

    val typeOf : Ty.V.var Prim.prim -> Ty.V.ty

  end = struct

    structure P = Prim

    val bTy = Ty.bool
    val i32Ty = Ty.raw RawTypes.T_Int
    val i64Ty = Ty.raw RawTypes.T_Long
    val f32Ty = Ty.raw RawTypes.T_Float
    val f64Ty = Ty.raw RawTypes.T_Double

    fun typeOf p = (case p
	   of P.isBoxed _ => bTy
	    | P.isUnboxed _ => bTy
	    | P.Equal _ => bTy
	    | P.NotEqual _ => bTy
	    | P.BNot _ => bTy
	    | P.BEq _ => bTy
	    | P.BNEq _ => bTy
	    | P.I32Add _ => i32Ty
	    | P.I32Sub _ => i32Ty
	    | P.I32Mul _ => i32Ty
	    | P.I32Div _ => i32Ty
	    | P.I32Mod _ => i32Ty
	    | P.I32ShiftLeft _ => i32Ty
	    | P.I32Neg _ => i32Ty
	    | P.I32Eq _ => i32Ty
	    | P.I32NEq _ => i32Ty
	    | P.I32Lt _ => i32Ty
	    | P.I32Lte _ => i32Ty
	    | P.I32Gt _ => i32Ty
	    | P.I32Gte _ => i32Ty
	    | P.I64Add _ => i64Ty
	    | P.I64Sub _ => i64Ty
	    | P.I64Mul _ => i64Ty
	    | P.I64Div _ => i64Ty
	    | P.I64Mod _ => i64Ty
	    | P.I64Neg _ => i64Ty
	    | P.I64Eq _ => i64Ty
	    | P.I64NEq _ => i64Ty
	    | P.I64Lt _ => i64Ty
	    | P.I64Lte _ => i64Ty
	    | P.I64Gt _ => i64Ty
	    | P.I64Gte _ => i64Ty
	    | P.F32Add _ => f32Ty
	    | P.F32Sub _ => f32Ty
	    | P.F32Mul _ => f32Ty
	    | P.F32Div _ => f32Ty
	    | P.F32Neg _ => f32Ty
	    | P.F32Sqrt _ => f32Ty
	    | P.F32Abs _ => f32Ty
	    | P.F32Eq _ => f32Ty
	    | P.F32NEq _ => f32Ty
	    | P.F32Lt _ => f32Ty
	    | P.F32Lte _ => f32Ty
	    | P.F32Gt _ => f32Ty
	    | P.F32Gte _ => f32Ty
	    | P.F64Add _ => f64Ty
	    | P.F64Sub _ => f64Ty
	    | P.F64Mul _ => f64Ty
	    | P.F64Div _ => f64Ty
	    | P.F64Neg _ => f64Ty
	    | P.F64Sqrt _ => f64Ty
	    | P.F64Abs _ => f64Ty
	    | P.F64Eq _ => f64Ty
	    | P.F64NEq _ => f64Ty
	    | P.F64Lt _ => f64Ty
	    | P.F64Lte _ => f64Ty
	    | P.F64Gt _ => f64Ty
	    | P.F64Gte _ => f64Ty
	    | P.I32ToI64X _ => i64Ty
	    | P.I32ToI64 _ => i64Ty
	    | P.I32ToF32 _ => f32Ty
	    | P.I32ToF64 _ => f64Ty
	    | P.I64ToF32 _ => f32Ty
	    | P.I64ToF64 _ => f64Ty
	    | P.F64ToI32 _ => i32Ty
	    | P.I32FetchAndAdd _ => i32Ty
	    | P.I64FetchAndAdd _ => i64Ty
	    | P.ArraySub (_, x) => Ty.V.typeOf x
	    | P.ArrayUpdate _ => bTy
	    | P.ArrayLength _ => i32Ty
	    | P.CAS(_, x, _) => Ty.V.typeOf x
	    | P.BCAS _ => bTy
	    | P.TAS _ => bTy
	  (* end case *))

  end
