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
	    name = "CPSOpt", help = "CPS-optimization controls"
	  }

  end
