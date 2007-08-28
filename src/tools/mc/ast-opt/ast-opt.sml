(* ast-opt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Currently, this phase is the identity, but we plan to add pattern-match
 * compilation and nested-parallelism flattening.
 *)

structure ASTOpt : sig

    val optimize : AST.module -> AST.module option

  end = struct

    fun optimize module = (* SOME module *)
	let val m' = FutParTup.futurize module
	in
	    PrintAST.print m';
	    SOME m'
	end

  end
