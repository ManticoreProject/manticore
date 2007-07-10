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
	    name = "BOMOpt", help = "BOM-optimization controls"
	  }

  end
