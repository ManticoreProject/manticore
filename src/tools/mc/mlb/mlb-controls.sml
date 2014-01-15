(* mlb-controls.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * MLB controls.
 *)

structure MLBControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "mlb",
	    pri = 3,
	    help = "controls for MLB processing"
	  }

  end
