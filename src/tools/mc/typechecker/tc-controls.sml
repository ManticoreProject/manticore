(* tc-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Controls for the typechecker
 *)

structure TCControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "tc",
	    pri = 1,
	    help = "Typechecker controls"
	  }

  end
