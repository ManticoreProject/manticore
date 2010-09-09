(* translate-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * AST to BOM translation controls.
 *)

structure TranslateControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "translate",
	    pri = 4,
	    help = "AST to BOM translation-phase controls"
	  }

    val keepEnv = Controls.genControl {
	    name = "keep-env",
	    pri = [5, 0],
	    obscurity = 1,
	    help = "keep the translation envronment",
	    default = false
	  }

    val _ = ControlRegistry.register registry {
	    ctl = Controls.stringControl ControlUtil.Cvt.bool keepEnv,
	    envName = NONE
	  }

  end
