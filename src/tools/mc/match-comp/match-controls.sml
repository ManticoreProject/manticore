(* match-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Pattern-match compiler controls.
 *)

structure MatchControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "match", help = "Pattern-match compiler controls"
	  }

  end
