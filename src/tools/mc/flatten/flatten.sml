(* flatten.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glue the pieces of the flattening transformation together.
 *)

structure Flatten : sig

  val flatten : AST.exp -> AST.exp

  val activeFlg : bool ref

end = struct

  val activeFlg = ref true

  fun flatten (e0 : AST.exp) : AST.exp = 
    if !activeFlg then let
      val _ = print "!@!@!@!@!@!@!@!@! FLATTENING TRANSFORMATION\n"
      val e1 = FlattenTerms.flatten e0
      val e2 = FlattenOpFusion.fuseExp e1
      val e3 = RealizeFArray.realize e2
      in
        e3
      end
    else e0

end
