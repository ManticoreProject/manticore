(* float.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Single-precision floating-point math functions
 *)

structure Float =
  struct

    structure PT = PrimTypes

  (* prototypes for external C functions *)
    _primcode(
	extern float M_Cosf (float) __attribute__((pure));
	extern float M_Powf (float, float) __attribute__((pure));
	extern float M_Sinf (float) __attribute__((pure));
	extern float M_Tanf (float) __attribute__((pure));
	extern void *M_FloatToString (float) __attribute__((alloc,pure));
    )

  (* HLOps that wrap C functions *)
    _primcode (
	define inline @float-cos (x : PT.ml_float / exh : PT.exh) : PT.ml_float =
	    let y : float = ccall M_Cosf (#0(x))
	    let res : PT.ml_float = alloc(y)
	      return (res);
	define inline @float-pow (arg : [PT.ml_float, PT.ml_float] / exh : PT.exh) : PT.ml_float =
	    let res : float = ccall M_Powf (#0 (#0(arg)), #0 (#1(arg)))
	      return (alloc(res));
	define inline @float-sin (x : PT.ml_float / exh : PT.exh) : PT.ml_float =
	    let res : float = ccall M_Sinf (#0(x))
	      return (alloc(res));
	define inline @float-sqrt (x : PT.ml_float / exh : PT.exh) : PT.ml_float =
	    let res : float = F32Sqrt (#0(x))
	      return (alloc(res));
	define inline @float-tan (x : PT.ml_float / exh : PT.exh) : PT.ml_float =
	    let res : float = ccall M_Tanf (#0(x))
	      return (alloc(res));
	define inline @to-string (f : PT.ml_float / exh : PT.exh) : PT.ml_string =
	    let res : PT.ml_string = ccall M_FloatToString (#0(f))
	      return (res)
	;
	define inline @from-int (f : PT.ml_int / exh : PT.exh) : PT.ml_float =
	    let res : PT.ml_float = alloc(I32ToF32 (#0(f)))
	      return (res)
	;
	define inline @from-long (f : PT.ml_long / exh : PT.exh) : PT.ml_float =
	    let res : PT.ml_float = alloc(I64ToF32 (#0(f)))
	      return (res)
	;

    )

  (* SML interface *)
    val cos : float -> float = _prim (@float-cos)
    val sin : float -> float = _prim (@float-sin)
    val tan : float -> float = _prim (@float-tan)
    val sqrt : float -> float = _prim (@float-sqrt)
    val pow : (float * float) -> float = _prim (@float-pow)
    val toString : float -> string = _prim(@to-string)
    val fromInt : int -> float = _prim(@from-int)
    val fromLong : long -> float = _prim(@from-long)

    fun max (x:float, y) = if x > y then x else y
    fun min (x:float, y) = if x < y then x else y

    fun compare (x : float, y) = 
	if x < y then LESS 
	else if x > y then GREATER
	else EQUAL

  end
