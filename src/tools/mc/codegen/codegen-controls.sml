(* codegen-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Codegen controls.
 *)

structure CodegenControls =
  struct

    val _ = BasicControl.nest (
	    MLRiscControl.prefix,
            MLRiscControl.registry,
            MLRiscControl.priority)

    val (registry, debug) = BasicControl.newRegistryWithDebug {
	    name = "codegen",
	    pri = 10,
	    help = "code-generation controls"
	  }

  end
