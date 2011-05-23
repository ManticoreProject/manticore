(* leaf-size.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Determine the maximum number of data elements M that can be stored at rope leaves
 * (i.e., the max leaf size) via the command line.
 *)

structure Range = struct

  val fail = Fail.fail "Range"

  (* nElts : int * int * int -> int *)
  fun nElts (from, to_, step) = (* "to" is syntax in pml *)
    if step = 0 then 
      fail "nElts" "cannot have step 0 in a range"
    else 
      1 + Int.max (0, (to_ - from) div step)

  fun mkRange (singleton, tabulate) = let
    (* note: both from and to_ are inclusive bounds *)
    fun range (from, to_, step) =  (* "to" is syntax in pml *)
      if from = to_ then 
        singleton from
      else let
        val sz = nElts (from, to_, step)
        fun gen n = from + step * n
        in
          tabulate (sz, gen)
        end
    in
      range
    end

end
