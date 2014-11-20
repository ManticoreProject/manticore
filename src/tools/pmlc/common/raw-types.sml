(* raw-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Raw machine-level numeric types.
 *)

structure RawTypes =
  struct

(* QUESTION: do we need/want the unsigned integer types? *)
    datatype raw_ty
      = Int8
      | UInt8
      | Int16
      | UInt16
      | Int32
      | UInt32
      | Int64
      | UInt64
      | Float32
      | Float64
      | Vec128		(* SSE *)
      | Vec256		(* AVX *)
      | Vec512		(* AVX-512 (Xeon Phi) *)

  (* size in bytes *)
    fun sizeOf Int8 = 1
      | sizeOf UInt8 = 1
      | sizeOf Int16 = 2
      | sizeOf UInt16 = 2
      | sizeOf Int32 = 4
      | sizeOf UInt32 = 4
      | sizeOf Int64 = 8
      | sizeOf UInt64 = 8
      | sizeOf Float32 = 4
      | sizeOf Float64 = 8
      | sizeOf Vec128 = 16
      | sizeOf Vec256 = 32
      | sizeOf Vec512 = 64

    fun toString Int8 = "int8"
      | toString UInt8 = "uint8"
      | toString Int16 = "int16"
      | toString UInt16 = "uint16"
      | toString Int32 = "int32"
      | toString UInt32 = "uint32"
      | toString Int64 = "int64"
      | toString UInt64 = "uint64"
      | toString Float32 = "float32"
      | toString Float64 = "float64"
      | toString Vec128 = "vec128"
      | toString Vec256 = "vec256"
      | toString Vec512 = "vec512"

    fun isInt Float32 = false
      | isInt Float64 = false
      | isInt Vec128 = false
      | isInt Vec256 = false
      | isInt Vec512 = false
      | isInt _ = true

    fun isFloat Float32 = true
      | isFloat Float64 = true
      | isFloat _ = false

    fun isVec Vec128 = true
      | isVec Vec256 = true
      | isVec Vec512 = true
      | isVec _ = false

  end
