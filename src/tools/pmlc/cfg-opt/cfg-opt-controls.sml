(* cfg-opt-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * CFG-optimization controls.
 *)

structure CFGOptControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "cfg",
	    pri = 9,
	    help = "controls for CFG optimization phases"
	  }

  end
