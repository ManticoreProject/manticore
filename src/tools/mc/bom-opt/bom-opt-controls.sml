(* bom-opt-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * BOMOpt controls.
 *)

structure BOMOptControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "bom",
	    pri = 5,
	    help = "controls for BOM optimization phases"
	  }

    val () = List.app (fn ctl => ControlRegistry.register registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = CFABOM.debugFlg,
                  name = "cfa-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug cfa"
                },
              Controls.control {
                  ctl = CFABOM.resultsFlg,
                  name = "cfa-results",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "print results of cfa"
                }
            ]
  end
