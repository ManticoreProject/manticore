(* prim-ty-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Determine the return type of primitive operators.
 *)

functor PrimTyFn (Ty : sig

    type var
    type ty

    val typeOf : var -> ty

    val noTy	: ty	(* used for return type of primops that have no results *)
    val boolTy  : ty
    val anyTy   : ty
    val raw     : RawTypes.raw_ty -> ty
    val addr    : ty -> ty

  end) : sig

    type var
    type ty

  (* the result type of a primop *)
    val typeOf : var Prim.prim -> ty

  (* the signature of a primop's arguments *)
    val signOf : var Prim.prim -> (ty list * ty)

  end = struct

    structure P = Prim

    type var = Ty.var
    type ty = Ty.ty

    val anyTy = Ty.anyTy
    val bTy = Ty.boolTy
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
	    | P.I32LSh _ => i32Ty
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
	    | P.ArrayLoadI32 _ => i32Ty
	    | P.ArrayLoadI64 _ => i64Ty
	    | P.ArrayLoadF32 _ => f32Ty
	    | P.ArrayLoadF64 _ => f64Ty
	    | P.ArrayLoad _ => Ty.anyTy
	    | P.ArrayStoreI32 _ => Ty.noTy
	    | P.ArrayStoreI64 _ => Ty.noTy
	    | P.ArrayStoreF32 _ => Ty.noTy
	    | P.ArrayStoreF64 _ => Ty.noTy
	    | P.ArrayStore _ => Ty.noTy
	    | P.I32FetchAndAdd _ => i32Ty
	    | P.I64FetchAndAdd _ => i64Ty
	    | P.CAS(_, x, _) => Ty.typeOf x
	    | P.BCAS _ => bTy
	    | P.TAS _ => bTy
	    | P.Pause => Ty.noTy
	    | P.FenceRead => Ty.noTy
	    | P.FenceWrite => Ty.noTy
	    | P.FenceRW => Ty.noTy
	  (* end case *))

    fun signOf p = (case p
	   of P.isBoxed _ => ([anyTy], bTy)
	    | P.isUnboxed _ => ([anyTy], bTy)
	    | P.Equal _ => ([anyTy, anyTy], bTy)
	    | P.NotEqual _ => ([anyTy, anyTy], bTy)
	    | P.BNot _ => ([bTy], bTy)
	    | P.BEq _ => ([bTy, bTy], bTy)
	    | P.BNEq _ => ([bTy, bTy], bTy)
	    | P.I32Add _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Sub _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Mul _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Div _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Mod _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32LSh _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Neg _ => ([i32Ty], i32Ty)
	    | P.I32Eq _ => ([i32Ty, i32Ty], bTy)
	    | P.I32NEq _ => ([i32Ty, i32Ty], bTy)
	    | P.I32Lt _ => ([i32Ty, i32Ty], bTy)
	    | P.I32Lte _ => ([i32Ty, i32Ty], bTy)
	    | P.I32Gt _ => ([i32Ty, i32Ty], bTy)
	    | P.I32Gte _ => ([i32Ty, i32Ty], bTy)
	    | P.I64Add _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Sub _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Mul _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Div _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Mod _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Neg _ => ([i64Ty], i64Ty)
	    | P.I64Eq _ => ([i64Ty, i64Ty], bTy)
	    | P.I64NEq _ => ([i64Ty, i64Ty], bTy)
	    | P.I64Lt _ => ([i64Ty, i64Ty], bTy)
	    | P.I64Lte _ => ([i64Ty, i64Ty], bTy)
	    | P.I64Gt _ => ([i64Ty, i64Ty], bTy)
	    | P.I64Gte _ => ([i64Ty, i64Ty], bTy)
	    | P.F32Add _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Sub _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Mul _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Div _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Neg _ => ([f32Ty], f32Ty)
	    | P.F32Sqrt _ => ([f32Ty], f32Ty)
	    | P.F32Abs _ => ([f32Ty], f32Ty)
	    | P.F32Eq _ => ([f32Ty, f32Ty], bTy)
	    | P.F32NEq _ => ([f32Ty, f32Ty], bTy)
	    | P.F32Lt _ => ([f32Ty, f32Ty], bTy)
	    | P.F32Lte _ => ([f32Ty, f32Ty], bTy)
	    | P.F32Gt _ => ([f32Ty, f32Ty], bTy)
	    | P.F32Gte _ => ([f32Ty, f32Ty], bTy)
	    | P.F64Add _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Sub _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Mul _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Div _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Neg _ => ([f64Ty], f64Ty)
	    | P.F64Sqrt _ => ([f64Ty], f64Ty)
	    | P.F64Abs _ => ([f64Ty], f64Ty)
	    | P.F64Eq _ => ([f64Ty, f64Ty], bTy)
	    | P.F64NEq _ => ([f64Ty, f64Ty], bTy)
	    | P.F64Lt _ => ([f64Ty, f64Ty], bTy)
	    | P.F64Lte _ => ([f64Ty, f64Ty], bTy)
	    | P.F64Gt _ => ([f64Ty, f64Ty], bTy)
	    | P.F64Gte _ => ([f64Ty, f64Ty], bTy)
	    | P.I32ToI64X _ => ([i32Ty], i64Ty)
	    | P.I32ToI64 _ => ([i32Ty], i64Ty)
	    | P.I32ToF32 _ => ([i32Ty], f32Ty)
	    | P.I32ToF64 _ => ([i32Ty], f64Ty)
	    | P.I64ToF32 _ => ([i64Ty], f32Ty)
	    | P.I64ToF64 _ => ([i64Ty], f64Ty)
	    | P.F64ToI32 _ => ([f64Ty], i32Ty)
	    | P.ArrayLoadI32 _ => ([anyTy, i32Ty], i32Ty)
	    | P.ArrayLoadI64 _ => ([anyTy, i32Ty], i64Ty)
	    | P.ArrayLoadF32 _ => ([anyTy, i32Ty], f32Ty)
	    | P.ArrayLoadF64 _ => ([anyTy, i32Ty], f64Ty)
	    | P.ArrayLoad _ => ([anyTy, i32Ty], Ty.anyTy)
	    | P.ArrayStoreI32 _ => ([anyTy, i32Ty, i32Ty], Ty.noTy)
	    | P.ArrayStoreI64 _ => ([anyTy, i32Ty, i64Ty], Ty.noTy)
	    | P.ArrayStoreF32 _ => ([anyTy, i32Ty, f32Ty], Ty.noTy)
	    | P.ArrayStoreF64 _ => ([anyTy, i32Ty, f64Ty], Ty.noTy)
	    | P.ArrayStore _ => ([anyTy, i32Ty, anyTy], Ty.noTy)
	    | P.I32FetchAndAdd _ => ([Ty.addr i32Ty, i32Ty], i32Ty)
	    | P.I64FetchAndAdd _ => ([Ty.addr i64Ty, i64Ty], i64Ty)
	    | P.CAS(_, x, _) => let
		val ty = Ty.typeOf x
		in
		  ([Ty.addr ty, ty, ty], ty)
		end
	    | P.BCAS(_, x, _) =>  let
		val ty = Ty.typeOf x
		in
		  ([Ty.addr ty, ty, ty], bTy)
		end
(* FIXME: what is the correct paramater type for TAS? *)
	    | P.TAS _ => ([anyTy], bTy)
	    | P.Pause => ([], Ty.noTy)
	    | P.FenceRead => ([], Ty.noTy)
	    | P.FenceWrite => ([], Ty.noTy)
	    | P.FenceRW => ([], Ty.noTy)
	  (* end case *))

  end
