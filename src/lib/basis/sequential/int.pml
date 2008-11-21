(* int.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Int =
  struct

    structure PT = PrimTypes

    type int = int

    _primcode (

      extern void *M_IntToString (int) __attribute__((alloc,pure));
      extern int M_CeilingLg (int) __attribute__((pure));

      define @to-string (n : ml_int / exh : exh) : ml_string =
	  let res : ml_string = ccall M_IntToString (unwrap(n))
	    return (res)
      ;

      define @ceiling-lg(n : ml_int / exh : exh) : ml_int =
	let res : int = ccall M_CeilingLg(unwrap(n))
	return (alloc(res))
      ;

    )

    val toString : int -> string = _prim(@to-string)
    val ceilingLg : int -> int = _prim(@ceiling-lg)

    fun max (x, y) = if x < y then y else x

    fun compare (x, y) = if x = y then EQUAL else if x < y then LESS else GREATER

  end
