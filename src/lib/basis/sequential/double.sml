(* double.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Double-precision floating-point math functions
 *)

structure Double =
  struct

  (* prototypes for external C functions *)
    _primcode(
	extern double cos (double) __attribute__((pure));
	extern double pow (double, double) __attribute__((pure));
	extern double sin (double) __attribute__((pure));
	extern double sqrt (double) __attribute__((pure));
	extern double tan (double) __attribute__((pure));
    )

  (* HLOps that wrap C functions *)
    _primcode (
	define @double-cos (x : ml_double / exh : exh) : ml_double =
	    let y : double = ccall cos (unwrap(x))
	    let res : ml_double = wrap(y)
	      return (res);
	define @double-pow (arg : [ml_double, ml_double] / exh : exh) : ml_double =
	    let res : double = ccall pow (unwrap (#0(arg)), unwrap (#1(arg)))
	      return (wrap(res));
	define @double-sin (x : ml_double / exh : exh) : ml_double =
	    let y : double = ccall sin (unwrap(x))
	      return (wrap(res));
	define @double-sqrt (x : ml_double / exh : exh) : ml_double =
	    let y : double = ccall sqrt (unwrap(x))
	      return (wrap(res));
	define @double-tan (x : ml_double / exh : exh) : ml_double =
	    let y : double = ccall tan (unwrap(x))
	      return (wrap(res));
    )

  (* SML interface *)
    val cos : double -> double = _prim (@double-cos)
    val sin : double -> double = _prim (@double-sin)
    val tan : double -> double = _prim (@double-tan)
    val sqrt : double -> double = _prim (@double-sqrt)
    val pow : (double * double) -> double = _prim (@double-pow)

  end
