(* parray-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure PArrayPair = struct

  val fail = Fail.fail "PArrayPair"

  fun map (f, p1, p2) = let
    val n = Int.min (PArray.length p1, PArray.length p2)
    fun f' i = f (p1 ! i, p2 ! i)
    in
      [| f' i | i in [| 0 to (n-1) |] |]
    end

end
