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

  (* replace parallel tuples with futures and touches *)
    val ptuples : AST.module -> AST.module = 
	  transform {passName = "ptuples", pass = FutParTup.futurize}

    fun optimize (module : AST.module) : AST.module = let
          val module = if (Controls.get BasicControl.sequential)
		then module
		else GrandPass.transform module
          in
            module
          end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintAST.output,
	    ext = "ast",
	    passName = "ast-optimize",
	    pass = optimize,
	    registry = ASTOptControls.registry
	  }

  end
