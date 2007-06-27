(* codegen-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Codegen controls.
 *)

structure CodegenControls =
struct
   val _ = BasicControl.nest (MLRiscControl.prefix,
                              MLRiscControl.registry,
                              MLRiscControl.priority)

   val registry = ControlRegistry.new {help = "Codegen"}
   val priority = []
   val _ = BasicControl.nest ("codegen", registry, priority)

   val debug =
      Controls.genControl
      {name = "debug",
       pri = priority,
       obscurity = BasicControl.debugPriority,
       help = "debug",
       default = false}
   val _ = 
      ControlRegistry.register
      registry
      {ctl = Controls.stringControl ControlUtil.Cvt.bool debug,
       envName = NONE}
end
