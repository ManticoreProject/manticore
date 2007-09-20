(* minmant-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Minmant controls.
 *)

structure MinmantControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "Minmant",
	    pri = 1,
	    help = "Minmant controls"
	  }

  end
