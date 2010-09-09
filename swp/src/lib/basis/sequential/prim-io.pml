(* prim-io.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive IO operations.
 *)

structure PrimIO =
  struct

    structure PT = PrimTypes

    _primcode(

      extern int M_ReadInt ();

      define @read-int (x : PT.unit / exh : PT.exh) : PT.ml_int =
	let y : int = ccall M_ReadInt ()
	let res : PT.ml_int = alloc(y)
	  return (res)
      ;

      extern double M_ReadDouble ();

      define @read-double (x : PT.unit / exh : PT.exh) : PT.ml_double =
	  let y : double = ccall M_ReadDouble ()
	  let res : PT.ml_double = alloc(y)
	    return (res)
      ;

    )

    val readInt : unit -> int = _prim(@read-int)
    val readDouble : unit -> double = _prim(@read-double)

  end
