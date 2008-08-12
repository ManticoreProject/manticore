(* print.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure Print =
  struct

    structure PT = PrimTypes

    _primcode(
      extern void M_Print(void*);
      define @print (s : String.ml_string / exh : PT.exh) : PT.unit =
	  let data : any = String.@data(s / exh)
	  do ccall M_Print (data)
	    return (UNIT)
      ;
    )

    val print : string -> unit = _prim(@print)

    fun printLn s = print(s^"\n")

  end
