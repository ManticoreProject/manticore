(* closure-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Closure-conversion controls.
 *)

structure ClosureControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "Closure", help = "closure-conversion controls"
	  }

  end
