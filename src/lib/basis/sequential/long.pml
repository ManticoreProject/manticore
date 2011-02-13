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
      extern void *M_LongFromString (void *) __attribute__((alloc,pure));

      define inline @to-string (n : PT.ml_long / exh : PT.exh) : PT.ml_string =
	  let res : PT.ml_string = ccall M_LongToString (unwrap(n))
	    return (res)
      ;

      define inline @from-string (s : ml_string / exh : exh) : Option.option =
	  let res : Option.option = ccall M_LongFromString (s)
	    return (res)
      ;

      define inline @to-int (n : ml_long / exh : exh) : ml_int =
	  return(alloc(I64ToI32(#0(n))))
	;

    )

    val toString : long -> string = _prim(@to-string)
    val fromString : string -> long Option.option = _prim(@from-string)
    val toInt : long -> int = _prim(@to-int)

  end
