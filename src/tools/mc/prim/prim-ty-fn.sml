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
    val anyTy   : ty
    val raw     : RawTypes.raw_ty -> ty
    val addr    : ty -> ty

  end) : sig

    type var
    type ty

  (* the result type of a primop *)
    val typeOf : var Prim.prim -> ty

  (* the signature of a primop *)
    val signOf : var Prim.prim -> (ty list * ty)

  (* the types of a condition's arguments *)
    val condArgTys : var Prim.cond -> ty list

  end = struct

    structure P = Prim

    type var = Ty.var
    type ty = Ty.ty

    val anyTy = Ty.anyTy
    val i8Ty  = Ty.raw RawTypes.T_Byte
    val i16Ty = Ty.raw RawTypes.T_Short
    val i32Ty = Ty.raw RawTypes.T_Int
    val i64Ty = Ty.raw RawTypes.T_Long
    val f32Ty = Ty.raw RawTypes.T_Float
    val f64Ty = Ty.raw RawTypes.T_Double
    val addrTy = Ty.addr anyTy

  (* the result type of a primop *)
    fun typeOf p = (case p
	   of P.I32Add _ => i32Ty
	    | P.I32Sub _ => i32Ty
	    | P.I32Mul _ => i32Ty
	    | P.I32Div _ => i32Ty
	    | P.I32Mod _ => i32Ty
	    | P.I32LSh _ => i32Ty
	    | P.I32Neg _ => i32Ty
	    | P.I64Add _ => i64Ty
	    | P.I64Sub _ => i64Ty
	    | P.I64Mul _ => i64Ty
	    | P.I64Div _ => i64Ty
	    | P.I64Mod _ => i64Ty
	    | P.I64LSh _ => i64Ty
	    | P.I64Neg _ => i64Ty
	    | P.U64Mul _ => i64Ty
	    | P.U64Div _ => i64Ty
	    | P.F32Add _ => f32Ty
	    | P.F32Sub _ => f32Ty
	    | P.F32Mul _ => f32Ty
	    | P.F32Div _ => f32Ty
	    | P.F32Neg _ => f32Ty
	    | P.F32Sqrt _ => f32Ty
	    | P.F32Abs _ => f32Ty
	    | P.F64Add _ => f64Ty
	    | P.F64Sub _ => f64Ty
	    | P.F64Mul _ => f64Ty
	    | P.F64Div _ => f64Ty
	    | P.F64Neg _ => f64Ty
	    | P.F64Sqrt _ => f64Ty
	    | P.F64Abs _ => f64Ty
	    | P.I32ToI64X _ => i64Ty
	    | P.I32ToI64 _ => i64Ty
	    | P.I32ToF32 _ => f32Ty
	    | P.I32ToF64 _ => f64Ty
	    | P.I64ToF32 _ => f32Ty
	    | P.I64ToF64 _ => f64Ty
	    | P.F64ToI32 _ => i32Ty
	    | P.AdrAddI32 _ => addrTy
	    | P.AdrAddI64 _ => addrTy
	    | P.AdrSubI32 _ => addrTy
	    | P.AdrSubI64 _ => addrTy
	    | P.AdrLoadI8 _ => i8Ty
	    | P.AdrLoadU8 _ => i8Ty
	    | P.AdrLoadI16 _ => i16Ty
	    | P.AdrLoadU16 _ => i16Ty
	    | P.AdrLoadI32 _ => i32Ty
	    | P.AdrLoadI64 _ => i64Ty
	    | P.AdrLoadF32 _ => f32Ty
	    | P.AdrLoadF64 _ => f64Ty
(* FIXME: this type should really be something like void* *)
	    | P.AdrLoadAdr _ => anyTy
	    | P.AdrLoad _ => anyTy
	    | P.AdrStoreI8 _ => Ty.noTy
	    | P.AdrStoreI16 _ => Ty.noTy
	    | P.AdrStoreI32 _ => Ty.noTy
	    | P.AdrStoreI64 _ => Ty.noTy
	    | P.AdrStoreF32 _ => Ty.noTy
	    | P.AdrStoreF64 _ => Ty.noTy
	    | P.AdrStoreAdr _ => Ty.noTy
	    | P.AdrStore _ => Ty.noTy
	    | P.ArrLoadI32 _ => i32Ty
	    | P.ArrLoadI64 _ => i64Ty
	    | P.ArrLoadF32 _ => f32Ty
	    | P.ArrLoadF64 _ => f64Ty
	    | P.ArrLoad _ => Ty.anyTy
	    | P.ArrStoreI32 _ => Ty.noTy
	    | P.ArrStoreI64 _ => Ty.noTy
	    | P.ArrStoreF32 _ => Ty.noTy
	    | P.ArrStoreF64 _ => Ty.noTy
	    | P.ArrStore _ => Ty.noTy
	    | P.I32FetchAndAdd _ => i32Ty
	    | P.I64FetchAndAdd _ => i64Ty
	    | P.CAS(_, x, _) => Ty.typeOf x
	    | P.Pause => Ty.noTy
	    | P.FenceRead => Ty.noTy
	    | P.FenceWrite => Ty.noTy
	    | P.FenceRW => Ty.noTy
	  (* end case *))

  (* the signature of a primop *)
    fun signOf p = (case p
	   of P.I32Add _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Sub _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Mul _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Div _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Mod _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32LSh _ => ([i32Ty, i32Ty], i32Ty)
	    | P.I32Neg _ => ([i32Ty], i32Ty)
	    | P.I64Add _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Sub _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Mul _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Div _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Mod _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64LSh _ => ([i64Ty, i64Ty], i64Ty)
	    | P.I64Neg _ => ([i64Ty], i64Ty)
	    | P.U64Mul _ => ([i64Ty, i64Ty], i64Ty)
	    | P.U64Div _ => ([i64Ty, i64Ty], i64Ty)
	    | P.F32Add _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Sub _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Mul _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Div _ => ([f32Ty, f32Ty], f32Ty)
	    | P.F32Neg _ => ([f32Ty], f32Ty)
	    | P.F32Sqrt _ => ([f32Ty], f32Ty)
	    | P.F32Abs _ => ([f32Ty], f32Ty)
	    | P.F64Add _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Sub _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Mul _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Div _ => ([f64Ty, f64Ty], f64Ty)
	    | P.F64Neg _ => ([f64Ty], f64Ty)
	    | P.F64Sqrt _ => ([f64Ty], f64Ty)
	    | P.F64Abs _ => ([f64Ty], f64Ty)
	    | P.I32ToI64X _ => ([i32Ty], i64Ty)
	    | P.I32ToI64 _ => ([i32Ty], i64Ty)
	    | P.I32ToF32 _ => ([i32Ty], f32Ty)
	    | P.I32ToF64 _ => ([i32Ty], f64Ty)
	    | P.I64ToF32 _ => ([i64Ty], f32Ty)
	    | P.I64ToF64 _ => ([i64Ty], f64Ty)
	    | P.F64ToI32 _ => ([f64Ty], i32Ty)
	    | P.AdrAddI32 _ => ([addrTy, i32Ty], addrTy)
	    | P.AdrAddI64 _ => ([addrTy, i64Ty], addrTy)
	    | P.AdrSubI32 _ => ([addrTy, i32Ty], addrTy)
	    | P.AdrSubI64 _ => ([addrTy, i64Ty], addrTy)
	    | P.AdrLoadI8 _ => ([Ty.addr i8Ty], i8Ty)
	    | P.AdrLoadU8 _ => ([Ty.addr i8Ty], i8Ty)
	    | P.AdrLoadI16 _ => ([Ty.addr i16Ty], i16Ty)
	    | P.AdrLoadU16 _ => ([Ty.addr i16Ty], i16Ty)
	    | P.AdrLoadI32 _ => ([Ty.addr i32Ty], i32Ty)
	    | P.AdrLoadI64 _ => ([Ty.addr i64Ty], i64Ty)
	    | P.AdrLoadF32 _ => ([Ty.addr f32Ty], f32Ty)
	    | P.AdrLoadF64 _ => ([Ty.addr f64Ty], f64Ty)
(* FIXME: this type should really be something like  void** -> void* *)
	    | P.AdrLoadAdr _ => ([addrTy], addrTy)
	    | P.AdrLoad _ => ([addrTy], anyTy)
	    | P.AdrStoreI8 _ => ([Ty.addr i8Ty, i8Ty], Ty.noTy)
	    | P.AdrStoreI16 _ => ([Ty.addr i16Ty, i16Ty], Ty.noTy)
	    | P.AdrStoreI32 _ => ([Ty.addr i32Ty, i32Ty], Ty.noTy)
	    | P.AdrStoreI64 _ => ([Ty.addr i64Ty, i64Ty], Ty.noTy)
	    | P.AdrStoreF32 _ => ([Ty.addr f32Ty, f32Ty], Ty.noTy)
	    | P.AdrStoreF64 _ => ([Ty.addr f64Ty, f64Ty], Ty.noTy)
(* FIXME: this type should really be something like  void** -> void* -> void *)
	    | P.AdrStoreAdr _ => ([addrTy, anyTy], Ty.noTy)
	    | P.AdrStore _ => ([addrTy, anyTy], Ty.noTy)
	    | P.ArrLoadI32 _ => ([anyTy, i32Ty], i32Ty)
	    | P.ArrLoadI64 _ => ([anyTy, i32Ty], i64Ty)
	    | P.ArrLoadF32 _ => ([anyTy, i32Ty], f32Ty)
	    | P.ArrLoadF64 _ => ([anyTy, i32Ty], f64Ty)
	    | P.ArrLoad _ => ([anyTy, i32Ty], Ty.anyTy)
	    | P.ArrStoreI32 _ => ([anyTy, i32Ty, i32Ty], Ty.noTy)
	    | P.ArrStoreI64 _ => ([anyTy, i32Ty, i64Ty], Ty.noTy)
	    | P.ArrStoreF32 _ => ([anyTy, i32Ty, f32Ty], Ty.noTy)
	    | P.ArrStoreF64 _ => ([anyTy, i32Ty, f64Ty], Ty.noTy)
	    | P.ArrStore _ => ([anyTy, i32Ty, anyTy], Ty.noTy)
	    | P.I32FetchAndAdd _ => ([Ty.addr i32Ty, i32Ty], i32Ty)
	    | P.I64FetchAndAdd _ => ([Ty.addr i64Ty, i64Ty], i64Ty)
	    | P.CAS(_, x, _) => let
		val ty = Ty.typeOf x
		in
		  ([Ty.addr ty, ty, ty], ty)
		end
	    | P.Pause => ([], Ty.noTy)
	    | P.FenceRead => ([], Ty.noTy)
	    | P.FenceWrite => ([], Ty.noTy)
	    | P.FenceRW => ([], Ty.noTy)
	  (* end case *))

    fun condArgTys c = (case c
	   of P.isBoxed _ => [anyTy]
	    | P.isUnboxed _ => [anyTy]
	    | P.Equal _ => [anyTy, anyTy]
	    | P.NotEqual _ => [anyTy, anyTy]
	    | P.EnumEq _ => [i32Ty, i32Ty]
	    | P.EnumNEq _ => [i32Ty, i32Ty]
	    | P.I32Eq _ => [i32Ty, i32Ty]
	    | P.I32NEq _ => [i32Ty, i32Ty]
	    | P.I32Lt _ => [i32Ty, i32Ty]
	    | P.I32Lte _ => [i32Ty, i32Ty]
	    | P.I32Gt _ => [i32Ty, i32Ty]
	    | P.I32Gte _ => [i32Ty, i32Ty]
	    | P.U32Lt _ => [i32Ty, i32Ty]
	    | P.I64Eq _ => [i64Ty, i64Ty]
	    | P.I64NEq _ => [i64Ty, i64Ty]
	    | P.I64Lt _ => [i64Ty, i64Ty]
	    | P.I64Lte _ => [i64Ty, i64Ty]
	    | P.I64Gt _ => [i64Ty, i64Ty]
	    | P.I64Gte _ => [i64Ty, i64Ty]
	    | P.U64Lt _ => [i64Ty, i64Ty]
	    | P.F32Eq _ => [f32Ty, f32Ty]
	    | P.F32NEq _ => [f32Ty, f32Ty]
	    | P.F32Lt _ => [f32Ty, f32Ty]
	    | P.F32Lte _ => [f32Ty, f32Ty]
	    | P.F32Gt _ => [f32Ty, f32Ty]
	    | P.F32Gte _ => [f32Ty, f32Ty]
	    | P.F64Eq _ => [f64Ty, f64Ty]
	    | P.F64NEq _ => [f64Ty, f64Ty]
	    | P.F64Lt _ => [f64Ty, f64Ty]
	    | P.F64Lte _ => [f64Ty, f64Ty]
	    | P.F64Gt _ => [f64Ty, f64Ty]
	    | P.F64Gte _ => [f64Ty, f64Ty]
	    | P.AdrEq _ => [addrTy, addrTy]
	    | P.AdrNEq _ => [addrTy, addrTy]
	    | P.BCAS(_, x, _) =>  let
		val ty = Ty.typeOf x
		in
		  [Ty.addr ty, ty, ty]
		end
	    | P.TAS _ => [Ty.addr i32Ty]
	  (* end case *))

  end
