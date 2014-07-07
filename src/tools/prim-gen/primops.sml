(* primops.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Primops : sig

    datatype ty
      = Bool
      | I32 | I64
      | F32 | F64

    type primop = {
	group : string,
	name : string,
	dom : ty list,
	rng : ty,
	pure : bool,
	cond : bool
      }

    val all : primop list

  end = struct

    datatype ty
      = Bool
      | I32 | I64
      | F32 | F64

    type primop = {
	group : string,
	name : string,
	dom : ty list,
	rng : ty,
	pure : bool,
	cond : bool
      }

    fun tyToSize Bool = "B"
      | tyToSize I32 = "I32"
      | tyToSize I64 = "I64"
      | tyToSize F32 = "F32"
      | tyToSize F64 = "F64"

    fun tyToPrefix Bool = "B"
      | tyToPrefix I32 = "I32"
      | tyToPrefix I64 = "I64"
      | tyToPrefix F32 = "F32"
      | tyToPrefix F64 = "F64"

  (* binary arithmetic operations *)
    fun mkBinOp (grp, prefix, ty) name = {
	    group = grp,
	    name = prefix ^ name,
	    dom = [ty, ty],
	    rng = ty,
	    pure = true,
	    cond = false
	  }
  (* unary arithmetic operations *)
    fun mkUnOp (grp, prefix, ty) name = {
	    group = grp,
	    name = prefix ^ name,
	    dom = [ty],
	    rng = ty,
	    pure = true,
	    cond = false
	  }
  (* binary relational operations *)
    fun mkRelOp (grp, prefix, ty) name = {
	    group = grp,
	    name = prefix ^ name,
	    dom = [ty, ty],
	    rng = Bool,
	    pure = true,
	    cond = true
	  }

    fun mkArithOps (grp, ty) = let
	  val prefix = tyToPrefix ty
	  val mkBinOp = mkBinOp (grp, prefix, ty)
	  val mkUnOp = mkUnOp (grp, prefix, ty)
	  val mkRelOp = mkRelOp (grp, prefix, ty)
	  in [
	    mkBinOp "Add",
	    mkBinOp "Sub",
	    mkBinOp "Mul",
	    mkBinOp "Div",
	    mkUnOp "Neg",
	    mkRelOp "Eq",
	    mkRelOp "NEq",
	    mkRelOp "Lt",
	    mkRelOp "Lte",
	    mkRelOp "Gt",
	    mkRelOp "Gte"
	  ] end

  (* additional integer operations *)
    fun mkIntOps (grp, ty) = let
	  val iPrefix = tyToPrefix ty
	  val uPrefix = "U" ^ tyToSize ty
	  in [
	    mkBinOp (grp, iPrefix, ty) "Mod",
	    mkBinOp (grp, uPrefix, ty) "Mul",
	    mkBinOp (grp, uPrefix, ty) "Div",
	    mkBinOp (grp, uPrefix, ty) "Rem",
	    mkBinOp (grp, uPrefix, ty) "LSh",
	    mkBinOp (grp, uPrefix, ty) "LOr",
	    mkBinOp (grp, uPrefix, ty) "LXor",
	    mkBinOp (grp, uPrefix, ty) "LAnd",
	    mkUnOp (grp, uPrefix, ty) "LNot",
	    mkRelOp (grp, "uPrefix", ty) "Lt"
	  ] end

    val all =
	  mkArithOps ("32-bit integer operations", I32) @
	  mkIntOps ("32-bit integer operations", I32) @
	  mkArithOps ("64-bit integer operations", I64) @
	  mkIntOps ("64-bit integer operations", I64) @
	  mkArithOps ("32-bit floating-point operations", I32) @
	  mkArithOps ("64-bit floating-point operations", I64) @
	  []

(***** BOM Primops
      = I32Add of 'var * 'var
      | I32Sub of 'var * 'var
      | I32Mul of 'var * 'var
      | I32Div of 'var * 'var
      | I32Mod of 'var * 'var
      | I32LSh of 'var * 'var
      | I32Neg of 'var
      | I64Add of 'var * 'var
      | I64Sub of 'var * 'var
      | I64Mul of 'var * 'var
      | I64Div of 'var * 'var
      | I64Mod of 'var * 'var
      | I64LSh of 'var * 'var
      | I64Neg of 'var
      | U64Mul of 'var * 'var
      | U64Div of 'var * 'var
      | U64Rem of 'var * 'var
      | F32Add of 'var * 'var
      | F32Sub of 'var * 'var
      | F32Mul of 'var * 'var
      | F32Div of 'var * 'var
      | F32Neg of 'var
      | F32Sqrt of 'var
      | F32Abs of 'var
      | F64Add of 'var * 'var
      | F64Sub of 'var * 'var
      | F64Mul of 'var * 'var
      | F64Div of 'var * 'var
      | F64Neg of 'var
      | F64Sqrt of 'var
      | F64Abs of 'var
    (* conversions *)
      | I32ToI64X of 'var		(* int -> long conversion with sign extension *)
      | I32ToI64 of 'var		(* unsigned int -> long conversion *)
      | I64ToI32 of 'var                (* int -> long conversion *)
      | I32ToF32 of 'var		(* int -> float conversion *)
      | I32ToF64 of 'var		(* int -> double conversion *)
      | I64ToF32 of 'var		(* long -> float conversion *)
      | I64ToF64 of 'var		(* long -> double conversion *)
      | F64ToI32 of 'var                (* double -> int conversion *)
    (* address arithmetic *)
      | AdrAddI32 of 'var * 'var
      | AdrAddI64 of 'var * 'var
      | AdrSubI32 of 'var * 'var
      | AdrSubI64 of 'var * 'var
    (* loads from addresses *)
      | AdrLoadI8 of 'var
      | AdrLoadU8 of 'var
      | AdrLoadI16 of 'var
      | AdrLoadU16 of 'var
      | AdrLoadI32 of 'var
      | AdrLoadI64 of 'var
      | AdrLoadF32 of 'var
      | AdrLoadF64 of 'var
      | AdrLoadAdr of 'var
      | AdrLoad of 'var                   (* load a uniform value from the given address *)
    (* stores to addresses *)
      | AdrStoreI8 of 'var * 'var
      | AdrStoreI16 of 'var * 'var
      | AdrStoreI32 of 'var * 'var
      | AdrStoreI64 of 'var * 'var
      | AdrStoreF32 of 'var * 'var
      | AdrStoreF64 of 'var * 'var
      | AdrStoreAdr of 'var * 'var
      | AdrStore of 'var * 'var           (* store a uniform value at the given address *)
    (* array load operations *)
      | ArrLoadI32 of 'var * 'var
      | ArrLoadI64 of 'var * 'var
      | ArrLoadF32 of 'var * 'var
      | ArrLoadF64 of 'var * 'var
      | ArrLoad of 'var * 'var	(* load a uniform value *)
    (* array store operations *)
      | ArrStoreI32 of 'var * 'var * 'var
      | ArrStoreI64 of 'var * 'var * 'var
      | ArrStoreF32 of 'var * 'var * 'var
      | ArrStoreF64 of 'var * 'var * 'var
      | ArrStore of 'var * 'var * 'var (* store a uniform value *)
    (* atomic operations *)
      | I32FetchAndAdd of 'var * 'var
      | I64FetchAndAdd of 'var * 'var
      | CAS of 'var * 'var * 'var	(* compare and swap; returns old value *)
    (* memory-system operations *)
      | Pause				(* yield processor to allow memory operations to be seen *)
      | FenceRead			(* memory fence for reads *)
      | FenceWrite			(* memory fence for writes *)
      | FenceRW				(* memory fence for both reads and writes *)
    (* allocation primitives *)
      | AllocPolyVec of 'var * 'var     (* AllocPolyVec (n, xs): allocate in the local heap a vector
					 * v of length n s.t. v[i] := l[i] for 0 <= i < n *)
      | AllocIntArray of 'var           (* allocates an array of ints in the local heap *)
      | AllocLongArray of 'var          (* allocates an array of longs in the local heap *)
      | AllocFloatArray of 'var         (* allocates an array of floats in the local heap *)
      | AllocDoubleArray of 'var        (* allocates an array of doubles in the local heap *)

  (* primitive conditional tests *)
    datatype 'var cond
      = isBoxed of 'var
      | isUnboxed of 'var
      | Equal of 'var * 'var	(* equality on T_any *)
      | NotEqual of 'var * 'var	(* equality on T_any *)
      | EnumEq of 'var * 'var
      | EnumNEq of 'var * 'var
      | I32Eq of 'var * 'var
      | I32NEq of 'var * 'var
      | I32Lt of 'var * 'var
      | I32Lte of 'var * 'var
      | I32Gt of 'var * 'var
      | I32Gte of 'var * 'var
      | U32Lt of 'var * 'var
      | I64Eq of 'var * 'var
      | I64NEq of 'var * 'var
      | I64Lt of 'var * 'var
      | I64Lte of 'var * 'var
      | I64Gt of 'var * 'var
      | I64Gte of 'var * 'var
      | U64Lt of 'var * 'var
      | F32Eq of 'var * 'var
      | F32NEq of 'var * 'var
      | F32Lt of 'var * 'var
      | F32Lte of 'var * 'var
      | F32Gt of 'var * 'var
      | F32Gte of 'var * 'var
      | F64Eq of 'var * 'var
      | F64NEq of 'var * 'var
      | F64Lt of 'var * 'var
      | F64Lte of 'var * 'var
      | F64Gt of 'var * 'var
      | F64Gte of 'var * 'var
      | AdrEq of 'var * 'var
      | AdrNEq of 'var * 'var
    (* conditional atomic operations *)
      | BCAS of 'var * 'var * 'var	(* compare and swap; returns bool *)
      | I32isSet of 'var		(* 32-bit test (for short-circuiting I32TAS) *)
      | I32TAS of 'var			(* 32-bit test and set *)
*****)

  end
