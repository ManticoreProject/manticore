(* ft-translate-controls.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * AST to BOM translation controls.
 *)

structure FTTranslateControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "ft-translate",
	    pri = 4,
	    help = "AST to FLAST, FLAST to BOM translation-phase controls"
	  }

    val flast = Controls.genControl {
            name = "flast",
	    pri = [0, 1],  (* not sure what this means *)
	    obscurity = 1, (* nor this *)
	    help = "turn on ast -> flast -> bom compilation",
	    default = false
          }

    val keepEnv = Controls.genControl {
	    name = "ft-keep-env",
	    pri = [5, 0],
	    obscurity = 1,
	    help = "keep the ft translation envronment",
	    default = false
	  }

    val _ = ControlRegistry.register registry {
            ctl = Controls.stringControl ControlUtil.Cvt.bool flast,
            envName = NONE
          }

    val _ = ControlRegistry.register registry {
	    ctl = Controls.stringControl ControlUtil.Cvt.bool keepEnv,
	    envName = NONE
	  }

  end
