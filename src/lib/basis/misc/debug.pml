(* debug.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * These debugging routines are for debugging the runtime libraries. They only have an effect when two
 * conditions are met.
 *
 *  - the compiler is compiled with the flag --enable-debug
 *
 *  - the Manticore program is compiled with the flag -Cdebug=true
 *
 * Common usage for printMsg is the DEBUG(msg) macro defined in include/debug.def.
 *)

structure Debug :
  sig

  (* prints a message when the program is in debug mode. *)
    val printMsg : (string * string * int) -> unit

  (* takes the failure condition, a file name and a file number, and prints this diagnostic information
   * and terminates the program.
   *)
    val assertFail : (string * string * int) -> unit

  end = struct

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
      extern void M_Die(void*);

      define @print (s : String.ml_string / exh : PT.exh) : PT.unit =
	  let data : any = String.@data(s / exh)
	  do ccall M_PrintDebug (data)
	  return (UNIT)
      ;

      define @print-msg (arg : [String.ml_string, String.ml_string, [int]]  / exh : PT.exh) : PT.unit =
	  let msg : any = String.@data(#0(arg) / exh)
	  let file : any = String.@data(#1(arg) / exh)
	  do ccall M_PrintDebugMsg (PT.false, msg, file, #0(#2(arg)))
	  return (UNIT)
      ;

      define @terminate-program (x : unit / exh : exh) : unit =
	do ccall M_Die("Failed assert")
	return(UNIT)
      ;

    )

    val print' : string -> unit = _prim(@print)
    val printMsg : (string * string * int) -> unit = _prim(@print-msg)
    val terminateProgram : unit -> unit = _prim(@terminate-program)

  (* takes the failure condition, a file name and a file number, and prints this diagnostic information
   * and terminates the program.
   *)
    fun assertFail (msg, file, lineNum) = (
	  printMsg(msg, file, lineNum);
	  Print.printLn "";
	  terminateProgram())

  end
