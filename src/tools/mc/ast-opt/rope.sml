(* rope.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Rope : sig

  val maxLeafSize  : unit -> int
  val ropeTyc      : unit -> Types.tycon
  val ropeTy       : Types.ty -> Types.ty
  val ropeLeaf     : unit -> AST.dcon
  val ropeCat      : unit -> AST.dcon

end = struct

  fun maxLeafSize () = Controls.get BasicControl.maxLeafSize
  val ropeTyc = DelayedBasis.TyCon.rope
  val ropeTy = DelayedBasis.Ty.rope
  val ropeLeaf = DelayedBasis.DataCon.ropeLeaf
  val ropeCat = DelayedBasis.DataCon.ropeCat 

end (* structure Rope *)
