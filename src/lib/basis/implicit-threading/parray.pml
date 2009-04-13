(* parray.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure PArray = struct

  (* FIXME too tightly coupled *)
    val sub = Ropes.sub
    val length = Ropes.length

  (* repP : int * 'a -> 'a parray *)
  (* called "dist" in NESL and Keller *)
  (* called "replicateP" in DPH impl *)
    fun repP (n, x) = Ropes.tabP (n, fn _ => x)

  (* toString : ('a -> string ) -> string -> 'a parray -> string *)
  (* FIXME: should we exploit the fact that we're dealing with a rope? *)
    fun toString eltToString sep parr = let
      val n = length parr
      fun lp (m, acc) =
       (if (m >= n) then
          List.rev ("|]" :: acc)
        else let
          val s = eltToString (sub (parr, m))
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
