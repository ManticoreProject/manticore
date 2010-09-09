(* manticore-region.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure ManticoreRegion : REGION = 
  struct

    type region = unit
    fun toString () = ""
    val memory = ()
    val stack = ()
    val readonly = ()
    val spill = ()

  end
