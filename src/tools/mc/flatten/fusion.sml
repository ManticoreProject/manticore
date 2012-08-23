(* inline.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flattening transformation in BOM.
 *
 *)

structure Fusion : sig

  val transform : BOM.module -> BOM.module

end = struct

  structure B  = BOM
      
  fun transform m = 
      if not(!BOMOptControls.flattenFlg) then m 
      else let
        val _ = TextIO.print "Flattening fusion phase."
        in
            m
        end

end
