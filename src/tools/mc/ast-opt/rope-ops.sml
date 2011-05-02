(* rope-ops.sml
 *
 * COPYRIGHT (c) 2008 Manticore Group
 * All rights reserved.
 *
 * Translate operators like "parray_sub" to "Rope.sub".
 *)

structure RopeOps : sig

  val tr : AST.var -> AST.var

end = struct

  structure B = Basis
  structure DV = DelayedBasis.Var

  fun tr x = 
    if Var.same (x, B.parray_sub) then
      DV.ropeSub ()
    else if Var.same (x, B.parray_len) then 
      DV.ropeLength ()
    else 
      x

end
