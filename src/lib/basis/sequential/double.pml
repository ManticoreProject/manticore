(* double.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Double-precision floating-point math functions
 *)

structure Double =
  struct

    structure PT = PrimTypes

  (* prototypes for external C functions *)
    _primcode(
	extern double M_Cos (double) __attribute__((pure));
	extern double M_Pow (double, double) __attribute__((pure));
	extern double M_Sin (double) __attribute__((pure));
	extern double M_Tan (double) __attribute__((pure));
	extern void *M_DoubleToString (double) __attribute__((alloc,pure));
    )

  (* HLOps that wrap C functions *)
    _primcode (
	define inline @double-cos (x : ml_double / exh : exh) : ml_double =
	    let y : double = ccall M_Cos (#0(x))
	    let res : ml_double = alloc(y)
	      return (res);
	define inline @double-pow (arg : [ml_double, ml_double] / exh : exh) : ml_double =
	    let res : double = ccall M_Pow (#0 (#0(arg)), #0 (#1(arg)))
	      return (alloc(res));
	define inline @double-sin (x : ml_double / exh : exh) : ml_double =
	    let res : double = ccall M_Sin (#0(x))
	      return (alloc(res));
	define inline @double-sqrt (x : ml_double / exh : exh) : ml_double =
	    let res : double = F64Sqrt (#0(x))
	      return (alloc(res));
	define inline @double-tan (x : ml_double / exh : exh) : ml_double =
	    let res : double = ccall M_Tan (#0(x))
	      return (alloc(res));
	define inline @to-string (f : ml_double / exh : exh) : ml_string =
	    let res : ml_string = ccall M_DoubleToString (#0(f))
	      return (res)
	;
	define inline @from-int (f : ml_int / exh : exh) : ml_double =
	    let res : ml_double = alloc(I32ToF64 (#0(f)))
	      return (res)
	;
      define inline @abs (f : ml_double / exh : exh) : ml_double =
	let res : ml_double = alloc(F64Abs(#0(f)))
          return (res)
      ;

    )

  (* SML interface *)
    val cos : double -> double = _prim (@double-cos)
    val sin : double -> double = _prim (@double-sin)
    val tan : double -> double = _prim (@double-tan)
    val sqrt : double -> double = _prim (@double-sqrt)
    val pow : (double * double) -> double = _prim (@double-pow)
    val toString : double -> string = _prim(@to-string)
    val fromInt : int -> double = _prim(@from-int)
    val abs : double -> double = _prim (@abs)

    fun max (x : double, y) = if x < y then y else x
    fun min (x : double, y) = if x < y then x else y

  end
