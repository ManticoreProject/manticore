(* translate-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * AST to BOM translation controls.
 *)

structure TranslateControls =
  struct

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "translate", help = "AST to BOM translation-phase controls"
	  }

  end
