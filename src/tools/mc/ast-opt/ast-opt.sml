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

    val futurize = BasicControl.mkKeepPassSimple {
	    output = PrintAST.output,
	    ext = "ast",
	    passName = "ptuples",
	    pass = FutParTup.futurize,
	    registry = ASTOptControls.registry
	  }

  (* futurize : AST.module -> AST.module *)
    fun futurize m = let
	  val m' = futurize m
	  in
	    m'
	  end

  (* optimize : AST.module -> AST.module *)
    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintAST.output,
	    ext = "ast",
	    passName = "optimize",
	    pass = futurize,
	    registry = ASTOptControls.registry
	  }

  end
