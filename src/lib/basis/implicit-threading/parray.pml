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

  (* toString : ('a -> string ) -> string -> 'a parray -> string *)
  (* FIXME: This seems to be segfaulting on inputs of 1000 or larger... *)
    fun toString eltToString sep parr = let
      val n = length parr
      fun build (m, acc) =
        if (m >= n) then
          acc
        else if (m = (n - 1)) then 
          build (m+1, acc ^ (eltToString (sub (parr, m))))
	else
	  build (m+1, acc ^ (eltToString (sub (parr, m))) ^ sep)
   in
     "[|" ^ (build (0, "")) ^ "|]"
   end

end
