(* parray.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel array utilities.
 *)

structure PArray = struct

  (* FIXME too tightly coupled *)
    val length = Ropes.length

  (* toString : ('a -> string ) * 'a parray -> string *)
    fun toString (eltToString, parr) = let
      val n = length parr
      fun build (m, acc) =
        if (m >= n) then
          acc
        else if (m = (n - 1)) then 
          build (m+1, acc ^ (eltToString (sub (v, m))))
	else
	  build (m+1, acc ^ (eltToString (sub (v, m))) ^ ",")
   in
     "[|" ^ (build (0, "")) ^ "|]"
   end

end
