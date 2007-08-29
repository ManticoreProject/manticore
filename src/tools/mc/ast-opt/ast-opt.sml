(* ast-opt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Currently, this phase is the identity, but we plan to add pattern-match
 * compilation and nested-parallelism flattening.
 *)

structure ASTOpt : sig

    val optimize : AST.module -> AST.module

  end = struct

    fun optimize module = let
          val module = FutParTup.futurize module
          in
            module
          end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintAST.output,
	    ext = "ast",
	    passName = "ASTOptimize",
	    pass = optimize,
	    registry = ASTOptControls.registry
	  }

  end
