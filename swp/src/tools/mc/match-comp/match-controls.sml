(* match-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Pattern-match compiler controls.
 *)

structure MatchControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "match",
	    pri = 3,
	    help = "Pattern-match compiler controls"
	  }

    val keepAST = Controls.genControl {
	    name = "keep-ast",
	    pri = [5, 0],
	    obscurity = 1,
	    help = "keep AST before and after match compilation",
	    default = false
	  }

    val _ = ControlRegistry.register registry {
	    ctl = Controls.stringControl ControlUtil.Cvt.bool keepAST,
	    envName = NONE
	  }

  end
