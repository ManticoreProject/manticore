(* prim-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a placeholder until we get primop generation working.  It includes
 * booleans, 32 and 64-bit integers, and 32 and 64-bit floating-point numbers.
 *)

structure PrimUtil : sig

    val nameOf : 'var Prim.prim -> string
    val fmt : ('var -> string) -> 'var Prim.prim -> string
    val varsOf : 'var Prim.prim -> 'var list
    val explode : 'var Prim.prim -> (('a list -> 'a Prim.prim) * 'var list)
    val map : ('a -> 'b) -> 'a Prim.prim -> 'b Prim.prim
    val app : ('a -> unit) -> 'a Prim.prim -> unit
    val isPure : 'var Prim.prim -> bool

  end = struct

    structure P = Prim

    fun nameOf (P.I32Add _) = "I32Add"
      | nameOf (P.I32Sub _) = "I32Sub"
      | nameOf (P.I32Mul _) = "I32Mul"
      | nameOf (P.I32Div _) = "I32Div"
      | nameOf (P.I32Mod _) = "I32Mod"
      | nameOf (P.I32LSh _) = "I32LSh"
      | nameOf (P.I32Neg _) = "I32Neg"
      | nameOf (P.I64Add _) = "I64Add"
      | nameOf (P.I64Sub _) = "I64Sub"
      | nameOf (P.I64Mul _) = "I64Mul"
      | nameOf (P.I64Div _) = "I64Div"
      | nameOf (P.I64Mod _) = "I64Mod"
      | nameOf (P.I64LSh _) = "I64LSh"
      | nameOf (P.I64Neg _) = "I64Neg"
      | nameOf (P.U64Mul _) = "U64Mul"
      | nameOf (P.U64Div _) = "U64Div"
      | nameOf (P.U64Rem _) = "U64Rem"
      | nameOf (P.F32Add _) = "F32Add"
      | nameOf (P.F32Sub _) = "F32Sub"
      | nameOf (P.F32Mul _) = "F32Mul"
      | nameOf (P.F32Div _) = "F32Div"
      | nameOf (P.F32Neg _) = "F32Neg"
      | nameOf (P.F32Sqrt _) = "F32Sqrt"
      | nameOf (P.F32Abs _) = "F32Abs"
      | nameOf (P.F64Add _) = "F64Add"
      | nameOf (P.F64Sub _) = "F64Sub"
      | nameOf (P.F64Mul _) = "F64Mul"
      | nameOf (P.F64Div _) = "F64Div"
      | nameOf (P.F64Neg _) = "F64Neg"
      | nameOf (P.F64Sqrt _) = "F64Sqrt"
      | nameOf (P.F64Abs _) = "F64Abs"
      | nameOf (P.I32ToI64X _) = "I32ToI64X"
      | nameOf (P.I32ToI64 _) = "I32ToI64"
      | nameOf (P.I64ToI32 _) = "I64ToI32"
      | nameOf (P.I32ToF32 _) = "I32ToF32"
      | nameOf (P.I32ToF64 _) = "I32ToF64"
      | nameOf (P.I64ToF32 _) = "I64ToF32"
      | nameOf (P.I64ToF64 _) = "I64ToF64"
      | nameOf (P.F64ToI32 _) = "F64ToI32"
      | nameOf (P.AdrAddI32 _) = "AdrAddI32"
      | nameOf (P.AdrAddI64 _) = "AdrAddI64"
      | nameOf (P.AdrSubI32 _) = "AdrSubI32"
      | nameOf (P.AdrSubI64 _) = "AdrSubI64"
      | nameOf (P.AdrLoadI8 _) = "AdrLoadI8"
      | nameOf (P.AdrLoadU8 _) = "AdrLoadU8"
      | nameOf (P.AdrLoadI16 _) = "AdrLoadI16"
      | nameOf (P.AdrLoadU16 _) = "AdrLoadU16"
      | nameOf (P.AdrLoadI32 _) = "AdrLoadI32"
      | nameOf (P.AdrLoadI64 _) = "AdrLoadI64"
      | nameOf (P.AdrLoadF32 _) = "AdrLoadF32"
      | nameOf (P.AdrLoadF64 _) = "AdrLoadF64"
      | nameOf (P.AdrLoadAdr _) = "AdrLoadAdr"
      | nameOf (P.AdrLoad _) = "AdrLoad"
      | nameOf (P.AdrStoreI8 _) = "AdrStoreI8"
      | nameOf (P.AdrStoreI16 _) = "AdrStoreI16"
      | nameOf (P.AdrStoreI32 _) = "AdrStoreI32"
      | nameOf (P.AdrStoreI64 _) = "AdrStoreI64"
      | nameOf (P.AdrStoreF32 _) = "AdrStoreF32"
      | nameOf (P.AdrStoreF64 _) = "AdrStoreF64"
      | nameOf (P.AdrStoreAdr _) = "AdrStoreAdr"
      | nameOf (P.AdrStore _) = "AdrStore"
      | nameOf (P.ArrLoadI32 _) = "ArrLoadI32"
      | nameOf (P.ArrLoadI64 _) = "ArrLoadI64"
      | nameOf (P.ArrLoadF32 _) = "ArrLoadF32"
      | nameOf (P.ArrLoadF64 _) = "ArrLoadF64"
      | nameOf (P.ArrLoad _) = "ArrLoad"
      | nameOf (P.ArrStoreI32 _) = "ArrStoreI32"
      | nameOf (P.ArrStoreI64 _) = "ArrStoreI64"
      | nameOf (P.ArrStoreF32 _) = "ArrStoreF32"
      | nameOf (P.ArrStoreF64 _) = "ArrStoreF64"
      | nameOf (P.ArrStore _) = "ArrStore"
      | nameOf (P.I32FetchAndAdd _) = "I32FetchAndAdd"
      | nameOf (P.I64FetchAndAdd _) = "I64FetchAndAdd"
      | nameOf (P.CAS _) = "CAS"
      | nameOf P.Pause = "Pause"
      | nameOf P.FenceRead = "FenceRead"
      | nameOf P.FenceWrite = "FenceWrite"
      | nameOf P.FenceRW = "FenceRW"
      | nameOf (P.AllocPolyVec _) = "AllocPolyVec"
      | nameOf (P.AllocLongArray _) = "AllocLongArray"

  (* return the list of variables referenced in a primitive operation *)
    fun varsOf (P.I32Add(a, b)) = [a, b]
      | varsOf (P.I32Sub(a, b)) = [a, b]
      | varsOf (P.I32Mul(a, b)) = [a, b]
      | varsOf (P.I32Div(a, b)) = [a, b]
      | varsOf (P.I32Mod(a, b)) = [a, b]
      | varsOf (P.I32LSh(a, b)) = [a, b]
      | varsOf (P.I32Neg a) = [a]
      | varsOf (P.I64Add(a, b)) = [a, b]
      | varsOf (P.I64Sub(a, b)) = [a, b]
      | varsOf (P.I64Mul(a, b)) = [a, b]
      | varsOf (P.I64Div(a, b)) = [a, b]
      | varsOf (P.I64Mod(a, b)) = [a, b]
      | varsOf (P.I64LSh(a, b)) = [a, b]
      | varsOf (P.I64Neg a) = [a]
      | varsOf (P.U64Mul(a, b)) = [a, b]
      | varsOf (P.U64Div(a, b)) = [a, b]
      | varsOf (P.U64Rem(a, b)) = [a, b]
      | varsOf (P.F32Add(a, b)) = [a, b]
      | varsOf (P.F32Sub(a, b)) = [a, b]
      | varsOf (P.F32Mul(a, b)) = [a, b]
      | varsOf (P.F32Div(a, b)) = [a, b]
      | varsOf (P.F32Neg a) = [a]
      | varsOf (P.F32Sqrt a) = [a]
      | varsOf (P.F32Abs a) = [a]
      | varsOf (P.F64Add(a, b)) = [a, b]
      | varsOf (P.F64Sub(a, b)) = [a, b]
      | varsOf (P.F64Mul(a, b)) = [a, b]
      | varsOf (P.F64Div(a, b)) = [a, b]
      | varsOf (P.F64Neg a) = [a]
      | varsOf (P.F64Sqrt a) = [a]
      | varsOf (P.F64Abs a) = [a]
      | varsOf (P.I32ToI64X a) = [a]
      | varsOf (P.I32ToI64 a) = [a]
      | varsOf (P.I64ToI32 a) = [a]
      | varsOf (P.I32ToF32 a) = [a]
      | varsOf (P.I32ToF64 a) = [a]
      | varsOf (P.I64ToF32 a) = [a]
      | varsOf (P.I64ToF64 a) = [a]
      | varsOf (P.F64ToI32 a) = [a]
      | varsOf (P.AdrAddI32(a, b)) = [a, b]
      | varsOf (P.AdrAddI64(a, b)) = [a, b]
      | varsOf (P.AdrSubI32(a, b)) = [a, b]
      | varsOf (P.AdrSubI64(a, b)) = [a, b]
      | varsOf (P.AdrLoadI8 a) = [a]
      | varsOf (P.AdrLoadU8 a) = [a]
      | varsOf (P.AdrLoadI16 a) = [a]
      | varsOf (P.AdrLoadU16 a) = [a]
      | varsOf (P.AdrLoadI32 a) = [a]
      | varsOf (P.AdrLoadI64 a) = [a]
      | varsOf (P.AdrLoadF32 a) = [a]
      | varsOf (P.AdrLoadF64 a) = [a]
      | varsOf (P.AdrLoadAdr a) = [a]
      | varsOf (P.AdrLoad a) = [a]
      | varsOf (P.AdrStoreI8(a, b)) = [a, b]
      | varsOf (P.AdrStoreI16(a, b)) = [a, b]
      | varsOf (P.AdrStoreI32(a, b)) = [a, b]
      | varsOf (P.AdrStoreI64(a, b)) = [a, b]
      | varsOf (P.AdrStoreF32(a, b)) = [a, b]
      | varsOf (P.AdrStoreF64(a, b)) = [a, b]
      | varsOf (P.AdrStoreAdr(a, b)) = [a, b]
      | varsOf (P.AdrStore(a, b)) = [a, b]
      | varsOf (P.ArrLoadI32(a, b)) = [a, b]
      | varsOf (P.ArrLoadI64(a, b)) = [a, b]
      | varsOf (P.ArrLoadF32(a, b)) = [a, b]
      | varsOf (P.ArrLoadF64(a, b)) = [a, b]
      | varsOf (P.ArrLoad(a, b)) = [a, b]
      | varsOf (P.ArrStoreI32(a, b, c)) = [a, b, c]
      | varsOf (P.ArrStoreI64(a, b, c)) = [a, b, c]
      | varsOf (P.ArrStoreF32(a, b, c)) = [a, b ,c]
      | varsOf (P.ArrStoreF64(a, b, c)) = [a, b, c]
      | varsOf (P.ArrStore(a, b, c)) = [a, b, c]
      | varsOf (P.I32FetchAndAdd(a, b)) = [a, b]
      | varsOf (P.I64FetchAndAdd(a, b)) = [a, b]
      | varsOf (P.CAS(a, b, c)) = [a, b, c]
      | varsOf P.Pause = []
      | varsOf P.FenceRead = []
      | varsOf P.FenceWrite = []
      | varsOf P.FenceRW = []
      | varsOf (P.AllocPolyVec (a, b)) = [a, b]
      | varsOf (P.AllocLongArray a) = [a]

    fun fmt v2s p = (case varsOf p
	   of [] => nameOf p ^ "()"
	    | [x] => concat[nameOf p, "(", v2s x, ")"]
	    | [x, y] => concat[nameOf p, "(", v2s x, ",", v2s y, ")"]
	    | [x, y, z] => concat[nameOf p, "(", v2s x, ",", v2s y, ",", v2s z, ")"]
	  (* end case *))

    local
      fun p0 p [] = p
	| p0 p _ = raise Fail "primop takes no args"
      fun p1 p [a] = p(a)
	| p1 p _ = raise Fail "unary primop needs one arg"
      fun p2 p [a, b] = p(a, b)
	| p2 p _ = raise Fail "binary primop needs two args"
      fun p3 p [a, b, c] = p(a, b, c)
	| p3 p _ = raise Fail "ternary primop needs three args"
    in
    fun explode (P.I32Add(a, b)) = (p2 P.I32Add, [a, b])
      | explode (P.I32Sub(a, b)) = (p2 P.I32Sub, [a, b])
      | explode (P.I32Mul(a, b)) = (p2 P.I32Mul, [a, b])
      | explode (P.I32Div(a, b)) = (p2 P.I32Div, [a, b])
      | explode (P.I32Mod(a, b)) = (p2 P.I32Mod, [a, b])
      | explode (P.I32LSh(a, b)) = (p2 P.I32LSh, [a, b])
      | explode (P.I32Neg a) = (p1 P.I32Neg, [a])
      | explode (P.I64Add(a, b)) = (p2 P.I64Add, [a, b])
      | explode (P.I64Sub(a, b)) = (p2 P.I64Sub, [a, b])
      | explode (P.I64Mul(a, b)) = (p2 P.I64Mul, [a, b])
      | explode (P.I64Div(a, b)) = (p2 P.I64Div, [a, b])
      | explode (P.I64Mod(a, b)) = (p2 P.I64Mod, [a, b])
      | explode (P.I64LSh(a, b)) = (p2 P.I64LSh, [a, b])
      | explode (P.I64Neg a) = (p1 P.I64Neg, [a])
      | explode (P.U64Mul(a, b)) = (p2 P.U64Mul, [a, b])
      | explode (P.U64Div(a, b)) = (p2 P.U64Div, [a, b])
      | explode (P.U64Rem(a, b)) = (p2 P.U64Rem, [a, b])
      | explode (P.F32Add(a, b)) = (p2 P.F32Add, [a, b])
      | explode (P.F32Sub(a, b)) = (p2 P.F32Sub, [a, b])
      | explode (P.F32Mul(a, b)) = (p2 P.F32Mul, [a, b])
      | explode (P.F32Div(a, b)) = (p2 P.F32Div, [a, b])
      | explode (P.F32Neg a) = (p1 P.F32Neg, [a])
      | explode (P.F32Sqrt a) = (p1 P.F32Sqrt, [a])
      | explode (P.F32Abs a) = (p1 P.F32Abs, [a])
      | explode (P.F64Add(a, b)) = (p2 P.F64Add, [a, b])
      | explode (P.F64Sub(a, b)) = (p2 P.F64Sub, [a, b])
      | explode (P.F64Mul(a, b)) = (p2 P.F64Mul, [a, b])
      | explode (P.F64Div(a, b)) = (p2 P.F64Div, [a, b])
      | explode (P.F64Neg a) = (p1 P.F64Neg, [a])
      | explode (P.F64Sqrt a) = (p1 P.F64Sqrt, [a])
      | explode (P.F64Abs a) = (p1 P.F64Abs, [a])
      | explode (P.I32ToI64X a) = (p1 P.I32ToI64X, [a])
      | explode (P.I32ToI64 a) = (p1 P.I32ToI64, [a])
      | explode (P.I64ToI32 a) = (p1 P.I64ToI32, [a])
      | explode (P.I32ToF32 a) = (p1 P.I32ToF32, [a])
      | explode (P.I32ToF64 a) = (p1 P.I32ToF64, [a])
      | explode (P.I64ToF32 a) = (p1 P.I64ToF32, [a])
      | explode (P.I64ToF64 a) = (p1 P.I64ToF64, [a])
      | explode (P.F64ToI32 a) = (p1 P.F64ToI32, [a])
      | explode (P.AdrAddI32(a, b)) = (p2 P.AdrAddI32, [a, b])
      | explode (P.AdrAddI64(a, b)) = (p2 P.AdrAddI64, [a, b])
      | explode (P.AdrSubI32(a, b)) = (p2 P.AdrSubI32, [a, b])
      | explode (P.AdrSubI64(a, b)) = (p2 P.AdrSubI64, [a, b])
      | explode (P.AdrLoadI8 a) = (p1 P.AdrLoadI8, [a])
      | explode (P.AdrLoadU8 a) = (p1 P.AdrLoadU8, [a])
      | explode (P.AdrLoadI16 a) = (p1 P.AdrLoadI16, [a])
      | explode (P.AdrLoadU16 a) = (p1 P.AdrLoadU16, [a])
      | explode (P.AdrLoadI32 a) = (p1 P.AdrLoadI32, [a])
      | explode (P.AdrLoadI64 a) = (p1 P.AdrLoadI64, [a])
      | explode (P.AdrLoadF32 a) = (p1 P.AdrLoadF32, [a])
      | explode (P.AdrLoadF64 a) = (p1 P.AdrLoadF64, [a])
      | explode (P.AdrLoadAdr a) = (p1 P.AdrLoadAdr, [a])
      | explode (P.AdrLoad a) = (p1 P.AdrLoad, [a])
      | explode (P.AdrStoreI8(a, b)) = (p2 P.AdrStoreI8, [a, b])
      | explode (P.AdrStoreI16(a, b)) = (p2 P.AdrStoreI16, [a, b])
      | explode (P.AdrStoreI32(a, b)) = (p2 P.AdrStoreI32, [a, b])
      | explode (P.AdrStoreI64(a, b)) = (p2 P.AdrStoreI64, [a, b])
      | explode (P.AdrStoreF32(a, b)) = (p2 P.AdrStoreF32, [a, b])
      | explode (P.AdrStoreF64(a, b)) = (p2 P.AdrStoreF64, [a, b])
      | explode (P.AdrStoreAdr(a, b)) = (p2 P.AdrStoreAdr, [a, b])
      | explode (P.AdrStore(a, b)) = (p2 P.AdrStore, [a, b])
      | explode (P.ArrLoadI32(a, b)) = (p2 P.ArrLoadI32, [a, b])
      | explode (P.ArrLoadI64(a, b)) = (p2 P.ArrLoadI64, [a, b])
      | explode (P.ArrLoadF32(a, b)) = (p2 P.ArrLoadF32, [a, b])
      | explode (P.ArrLoadF64(a, b)) = (p2 P.ArrLoadF64, [a, b])
      | explode (P.ArrLoad(a, b)) = (p2 P.ArrLoad, [a, b])
      | explode (P.ArrStoreI32(a, b, c)) = (p3 P.ArrStoreI32, [a, b, c])
      | explode (P.ArrStoreI64(a, b, c)) = (p3 P.ArrStoreI64, [a, b, c])
      | explode (P.ArrStoreF32(a, b, c)) = (p3 P.ArrStoreF32, [a, b, c])
      | explode (P.ArrStoreF64(a, b, c)) = (p3 P.ArrStoreF64, [a, b, c])
      | explode (P.ArrStore(a, b, c)) = (p3 P.ArrStore, [a, b, c])
      | explode (P.I32FetchAndAdd(a, b)) = (p2 P.I32FetchAndAdd, [a, b])
      | explode (P.I64FetchAndAdd(a, b)) = (p2 P.I64FetchAndAdd, [a, b])
      | explode (P.CAS(a, b, c)) = (p3 P.CAS, [a, b, c])
      | explode P.Pause = (p0 P.Pause, [])
      | explode P.FenceRead = (p0 P.FenceRead, [])
      | explode P.FenceWrite = (p0 P.FenceWrite, [])
      | explode P.FenceRW = (p0 P.FenceRW, [])
      | explode (P.AllocPolyVec (a, b)) = (p2 P.AllocPolyVec, [a, b])
      | explode (P.AllocLongArray a) = (p1 P.AllocLongArray, [a])
    end (* local *)

    fun map f p = let val (mk, args) = explode p in mk(List.map f args) end

    fun app f p = List.app f (varsOf p)

    fun isPure (P.AdrStoreI8 _) = false
      | isPure (P.AdrStoreI16 _) = false
      | isPure (P.AdrStoreI32 _) = false
      | isPure (P.AdrStoreI64 _) = false
      | isPure (P.AdrStoreF32 _) = false
      | isPure (P.AdrStoreF64 _) = false
      | isPure (P.AdrStoreAdr _) = false
      | isPure (P.AdrStore _) = false
      | isPure (P.ArrStoreI32 _) = false
      | isPure (P.ArrStoreI64 _) = false
      | isPure (P.ArrStoreF32 _) = false
      | isPure (P.ArrStoreF64 _) = false
      | isPure (P.ArrStore _) = false
      | isPure (P.I32FetchAndAdd _) = false
      | isPure (P.I64FetchAndAdd _) = false
      | isPure (P.CAS _) = false
      | isPure P.Pause = false
      | isPure P.FenceRead = false
      | isPure P.FenceWrite = false
      | isPure P.FenceRW = false
      | isPure _ = true

  end
