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
	define @double-cos (x : PT.ml_double / exh : PT.exh) : PT.ml_double =
	    let y : double = ccall M_Cos (#0(x))
	    let res : PT.ml_double = alloc(y)
	      return (res);
	define @double-pow (arg : [PT.ml_double, PT.ml_double] / exh : PT.exh) : PT.ml_double =
	    let res : double = ccall M_Pow (#0 (#0(arg)), #0 (#1(arg)))
	      return (alloc(res));
	define @double-sin (x : PT.ml_double / exh : PT.exh) : PT.ml_double =
	    let res : double = ccall M_Sin (#0(x))
	      return (alloc(res));
	define @double-sqrt (x : PT.ml_double / exh : PT.exh) : PT.ml_double =
	    let res : double = F64Sqrt (#0(x))
	      return (alloc(res));
	define @double-tan (x : PT.ml_double / exh : PT.exh) : PT.ml_double =
	    let res : double = ccall M_Tan (#0(x))
	      return (alloc(res));
	define @to-string (f : PT.ml_double / exh : PT.exh) : PT.ml_string =
	    let res : PT.ml_string = ccall M_DoubleToString (#0(f))
	      return (res)
	;
	define @from-int (f : PT.ml_int / exh : PT.exh) : PT.ml_double =
	    let res : PT.ml_double = alloc(I32ToF64 (#0(f)))
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

  end
