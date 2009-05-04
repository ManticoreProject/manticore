(* parray.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure PArray = struct

    type 'a parray = 'a Ropes.rope

  (* FIXME too tightly coupled *)
    val subP = Ropes.sub
    val lengthP = Ropes.length
    val reduceP = Ropes.reduceP
    val filterP = Ropes.filterP
    val mapP = Ropes.mapP
    val revP = Ropes.revP
    val fromListP = Ropes.fromList
    val concatP = Ropes.concat

  (* repP : int * 'a -> 'a parray *)
  (* called "dist" in NESL and Keller *)
  (* called "replicateP" in DPH impl *)
    fun repP (n, x) = Ropes.tabP (n, fn _ => x)

  (* toString : ('a -> string ) -> string -> 'a parray -> string *)
  (* FIXME: should we exploit the fact that we're dealing with a rope? *)
    fun toString eltToString sep parr = let
      val n = lengthP parr
      fun lp (m, acc) =
       (if (m >= n) then
          List.rev ("|]" :: acc)
        else let
          val s = eltToString (subP (parr, m))
          in
            if (m = (n-1)) then
              List.rev ("|]" :: s :: acc)
            else
              lp (m+1, sep :: s :: acc)
          end)
      val init = "[|" :: nil
      in
        String.concat (lp (0, init))
      end

end

(* below is the subset of the parallel array module that should bound at the top level. *)
type 'a parray = 'a PArray.parray
val reduceP = PArray.reduceP
val filterP = PArray.filterP
val subP = PArray.subP
val revP = PArray.revP
val lengthP = PArray.lengthP
val mapP = PArray.mapP
val fromListP = PArray.fromListP
val concatP = PArray.concatP
