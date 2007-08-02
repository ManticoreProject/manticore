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
	  print(concat["parsing ", file, "\n"]);
	  case Parser.parseFile (errStrm, file)
	   of SOME pt => (
		print(concat["typechecking ", file, "\n"]);
		Typechecker.check (errStrm, pt))
	    | NONE => NONE
	  (* end case *))

  end

