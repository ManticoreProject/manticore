(* debug.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Debug =
  struct

    structure PT = PrimTypes

    _primcode (
      extern void M_Print (void*);
      extern void M_PrintInt (int);
      extern void M_PrintPtr (void *, void *);
      extern void M_PrintDebug (void*);
      extern void M_PrintLong (long);
      extern void M_PrintDebugMsg (void*, void*, void*, int);
      extern void M_PrintTestingMsg (void*, void*, int);

      extern void M_Print(void*);
      define @print (s : String.ml_string / exh : PT.exh) : PT.unit =
	  let data : any = String.@data(s / exh)
	  do ccall M_PrintDebug (data)
	  return (UNIT)
      ;

      define @print-msg (arg : [String.ml_string, String.ml_string, [int]]  / exh : PT.exh) : PT.unit =
	  let msg : any = String.@data(#0(arg) / exh)
	  let file : any = String.@data(#1(arg) / exh)
	  do ccall M_PrintDebugMsg (PT.TRUE, msg, file, #0(#2(arg)))
	  return (UNIT)
      ;

    )

    val print' : string -> unit = _prim(@print)
    fun print s = print'(s^"\n")
    val printMsg : (string * string * int) -> unit = _prim(@print-msg)

  end
