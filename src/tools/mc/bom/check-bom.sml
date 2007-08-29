(* check-bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CheckBOM : sig

    val check : BOM.module -> unit

  end = struct

    fun check (m : BOM.module) = ()

    val check =
       BasicControl.mkTracePass
       {passName = "BOMCheck",
        pass = check,
        verbose = 2}

  end
