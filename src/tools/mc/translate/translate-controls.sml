(* codegen-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * BOMOpt controls.
 *)

structure TranslateControls =
struct
   val registry = ControlRegistry.new {help = "Translate"}
   val priority = []
   val _ = BasicControl.nest ("Translate", registry, priority)

   val debug =
      Controls.genControl
      {name = "debug",
       pri = priority,
       obscurity = BasicControl.debugObscurity,
       help = "debug",
       default = false}
   val _ = 
      ControlRegistry.register
      registry
      {ctl = Controls.stringControl ControlUtil.Cvt.bool debug,
       envName = NONE}
end
