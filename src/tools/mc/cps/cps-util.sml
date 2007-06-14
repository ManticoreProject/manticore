(* cps-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPSUtil : sig

    val rhsToString : CPS.rhs -> string

  end = struct

    structure C = CPS

    val v2s = C.Var.toString
    fun vl2s xs = String.concatWith ", " (List.map v2s xs)
    val p2s = PrimUtil.fmt v2s

    fun rhsToString (C.Var xs) = concat["Var(", vl2s xs, ")"]
      | rhsToString (C.Cast(ty, x)) = concat["Cast(", CPSTy.toString ty, ", ", v2s x, ")"]
      | rhsToString (C.Const(lit, ty)) = concat["Const(", Literal.toString lit, ", ", CPSTy.toString ty, ")"]
      | rhsToString (C.Select(i, x)) = concat["Select(", Int.toString i, ", ", v2s x, ")"]
      | rhsToString (C.Update(i, x, y)) = concat["Update(", Int.toString i, ", ", v2s x,  ", ", v2s y, ")"]
      | rhsToString (C.AddrOf(i, x)) = concat["AddrOf(", Int.toString i, ", ", v2s x, ")"]
      | rhsToString (C.Alloc xs) = concat["Alloc(", vl2s xs, ")"]
      | rhsToString (C.Wrap x) = concat["Wrap(", v2s x, ")"]
      | rhsToString (C.Unwrap x) = concat["Unwrap(", v2s x, ")"]
      | rhsToString (C.Prim p) = p2s p
      | rhsToString (C.CCall(cf, xs)) = concat["CCall(", v2s cf, ", [", vl2s xs, "])"]
      | rhsToString (C.HostVProc) = "HostVProc"
      | rhsToString (C.VPLoad(n, x)) = concat["VPLoad(", IntInf.toString n, ", ", v2s x, ")"]
      | rhsToString (C.VPStore(n, x, y)) = concat["VPStore(", IntInf.toString n, v2s x,  ", ", v2s y, ")"]

  end
