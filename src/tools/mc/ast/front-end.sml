(* front-end.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure FrontEnd : sig

    val load : (Error.err_stream * string) -> AST.module option

  end = struct

  (* parse and typecheck a file *)
    fun load (errStrm, file) = (
	  case Parser.parseFile (errStrm, file)
	   of SOME pt => SOME(Typechecker.check (errStrm, pt))
	    | NONE => NONE
	  (* end case *))

    val load = BasicControl.mkTracePassSimple {passName = "load", pass = load}

  end

