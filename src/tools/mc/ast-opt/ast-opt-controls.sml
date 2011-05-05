(* ast-opt-controls.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * AST-optimization controls.
 *)

structure ASTOptControls = struct

  structure C  = Controls
  structure CR = ControlRegistry
  structure CU = ControlUtil

  val (registry, debug) = BasicControl.newRegistryWithDebug {
    name = "ast",
    pri = 2,
    help = "controls for AST transformation phases"
  }

  val inlineRangesFlag = ref true

  fun reg ctl = let
    val arg = {ctl = C.stringControl CU.Cvt.bool ctl, envName = NONE}
    in
      CR.register registry arg
    end
  
  val inlineRangesCtl = C.control {
    ctl = inlineRangesFlag,
    name = "inline-ranges-on",
    pri = [0, 1],
    obscurity = 0,
    help = "turn on/off inlining of range expressions"
  }
  
  val _ = List.app reg [inlineRangesCtl]

end
