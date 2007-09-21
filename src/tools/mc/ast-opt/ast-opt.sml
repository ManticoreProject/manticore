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

  (* a wrapper for AST optimization passes *)
    fun transform {passName, pass} = BasicControl.mkKeepPassSimple {
	    output = PrintAST.output,
	    ext = "ast",
	    passName = passName,
	    pass = pass,
	    registry = ASTOptControls.registry
	  }


  (* futParTup : AST.module -> AST.module *)
    val futParTup = transform {passName = "fut-par-tup", pass = FutParTup.futurize}

  (* futurize : AST.module -> AST.module *)
    fun futurize m = let
	  val m' = futParTup m
	  in
	    m'
	  end

    val futurize = transform {passName = "futurize", pass = futurize}

  (* optimize : AST.module -> AST.module *)
    fun optimize module = let
          val module = futurize module
          in
            module
          end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintAST.output,
	    ext = "ast",
	    passName = "optimize",
	    pass = optimize,
	    registry = ASTOptControls.registry
	  }

  end
