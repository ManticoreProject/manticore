(* inline.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Flattening transformation in BOM.
 *
 *)

structure Flatten : sig

  val transform : BOM.module -> BOM.module

end = struct

  fun transform m = 
    if not(!BOMOptControls.flattenFlg) then m 
    else let
      val _ = TextIO.print "The compiler *would* be flattening now.\n"
      in
        m
      end

end
