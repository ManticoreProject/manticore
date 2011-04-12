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
      val _ = print "-- BEGINNING FLATTENING TRANSFORMATION\n"
      val (e1, flatTycs) = FlattenTerms.flatten e0
      val e2 = FlattenOpFusion.fuseExp e1
      val e3 = RealizeTerms.realize (e2, flatTycs)
      in
        e3
      end
    else e0

end
