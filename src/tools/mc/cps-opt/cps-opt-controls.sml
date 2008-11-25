(* codegen-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * CPS optimization controls.
 *)

structure CPSOptControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "cps",
	    pri = 7,
	    help = "controls for CPS optimization phases"
	  }

    val () = List.app (fn ctl => ControlRegistry.register registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = CFACPS.debugFlg,
                  name = "cfa-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug cfa"
                },
              Controls.control {
                  ctl = CFACPS.resultsFlg,
                  name = "cfa-results",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "print results of cfa"
                }
            ]

  end
