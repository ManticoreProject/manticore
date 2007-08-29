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
	    name = "Minmant", help = "Minmant controls"
	  }

  end
