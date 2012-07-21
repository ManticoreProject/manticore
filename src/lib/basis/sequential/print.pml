(* print.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Print =
  struct

    structure PT = PrimTypes

    _primcode(
      extern void M_Print (void*);
      define inline @print (s : ml_string / exh : exh) : unit =
	  let data : any = String.@data(s / exh)
	  do ccall M_Print (data)
	    return (UNIT)
        ;

      extern void M_PrintOrd (int);
      define inline @printOrd (i : ml_int / exh : exh) : unit =
	  let data : int = #0(i)
	  do ccall M_PrintOrd (data)
	    return (UNIT)
        ;
    )

    val print : string -> unit = _prim(@print)
    val printOrd : int -> unit = _prim(@printOrd)

    fun printLn s = print(s^"\n")

  end
