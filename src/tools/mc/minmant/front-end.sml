(* front-end.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure FrontEnd : sig

    val load : string -> AST.module option

  end = struct

  (* parse and typecheck a file *)
    fun load file = (
	  print(concat["parsing ", file, "\n"]);
	  case Parser.parseFile file
	   of SOME pt => (
		print(concat["typechecking ", file, "\n"]);
		Typechecker.check pt)
	    | NONE => NONE
	  (* end case *))

  end

