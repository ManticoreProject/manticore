(* long.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Long =
  struct

    structure PT = PrimTypes

    type long = long

    _primcode (

      extern void *M_LongToString (long) __attribute__((alloc,pure));

      define inline @to-string (n : PT.ml_long / exh : PT.exh) : PT.ml_string =
	  let res : PT.ml_string = ccall M_LongToString (unwrap(n))
	    return (res)
      ;

    )

    val toString : long -> string = _prim(@to-string)

  end
