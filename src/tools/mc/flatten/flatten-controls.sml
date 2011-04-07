(* flatten-controls.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Flattening transformation controls.
 *)

structure FlattenControls = struct

(* FIXME not working
  structure C  = Controls
  structure CR = ControlRegistry
  structure CU = ControlUtil 

  val (registry, debug) = BasicControl.newRegistryWithDebug {
			  name = "flattening-trans",
			  pri = 7,
			  help = "controls for flattening transformation"
			  }

  fun reg ctl = let
    val arg = {ctl = C.stringControl CU.Cvt.bool ctl,
	       envName = NONE}
    in
      CR.register registry arg
    end
  
  val activateCtl = C.control {ctl = Flatten.activeFlg,
			       name = "enabled",
			       pri = [0, 1],
			       obscurity = 0,
			       help = "enable the flattening transformation"}
  
  val () = List.app reg [activateCtl]
*)

end
