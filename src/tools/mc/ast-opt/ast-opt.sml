(* ast-opt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Currently, this phase is the identity, but we plan to add pattern-match
 * compilation and nested-parallelism flattening.
 *)

structure ASTOpt : sig

    val optimize : AST.exp -> AST.exp

  end = struct

  (* a wrapper for AST optimization passes *)
    fun transform {passName, pass} = BasicControl.mkKeepPassSimple {
	    output = PrintAST.outputExp,
	    ext = "ast",
	    passName = passName,
	    pass = pass,
	    registry = ASTOptControls.registry
	  }

  (* replace parallel tuples with futures and touches *)
(*    val ptuples : AST.module -> AST.module = 
	  transform {passName = "ptuples", pass = FutParTup.futurize}
*)

    val pvals : AST.exp -> AST.exp =
	transform {passName="pval-to-future", pass=PValToFuture.tr}

    val pvalToFuture = Controls.genControl {
	    name = "pval-to-future",
	    pri = [5, 0],
	    obscurity = 1,
	    help = "Translate pvals to futures.",
	    default = false
	  }

(* FIXME: use expansion option instead of this compiler flag *)
    val _ = ControlRegistry.register ASTOptControls.registry {
	    ctl = Controls.stringControl ControlUtil.Cvt.bool pvalToFuture,
	    envName = NONE}

    fun optimize (exp : AST.exp) : AST.exp = let
	  val exp = if (Controls.get BasicControl.sequential)
		          then Unpar.unpar exp
		          else let
			    val exp = if Controls.get pvalToFuture
				          then pvals exp
				      else exp
			    val exp = GrandPass.transform exp
			    in
				exp
			    end
          in
            exp
          end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintAST.outputExp,
	    ext = "ast",
	    passName = "ast-optimize",
	    pass = optimize,
	    registry = ASTOptControls.registry
	  }

  end
