(* front-end.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure FrontEnd : sig

    val load : string -> AST.module option

  end = struct

  (* parse and typecheck a file *)
    fun load file = Option.mapPartial Typechecker.check (Parser.parseFile file)

  end

