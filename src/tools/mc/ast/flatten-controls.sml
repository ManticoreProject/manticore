(* flatten-controls.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Flattening transformation controls.
 *)

structure FlattenControls = struct

  structure C  = Controls
  structure CR = ControlRegistry
  structure CU = ControlUtil 

  val onFlg = ref true

  val (registry, debug) = BasicControl.newRegistryWithDebug {
    name = "flattening-transformation",
    pri = 3,
    help = "controls for flattening transformation"
  }

  fun reg ctl = let
    val arg = {ctl = C.stringControl CU.Cvt.bool ctl, envName = NONE}
    in
      CR.register registry arg
    end
  
  val onCtl = C.control {
    ctl = onFlg,
    name = "on",
    pri = [0, 1],
    obscurity = 0,
    help = "turn on the flattening transformation"
  }
  
  val () = List.app reg [onCtl]

end
