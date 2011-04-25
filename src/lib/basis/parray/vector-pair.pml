(* vector-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure VectorPair = struct

  structure VS = VectorSeq

  fun tabulate (n, f) = VS.unzip (VS.tabulate (n, f))
 
  fun map f (v1, v2) = let
    val n1 = VS.length v1
    val n2 = VS.length v2
    in
      VS.tabulate (if (n1<n2) then n1 else n2, 
		   fn i => f (VS.sub (v1, i), VS.sub (v2, i)))
    end

  fun mapEq f (v1, v2) = let
    val n1 = VS.length v1
    val n2 = VS.length v2
    in
      if (n1 = n2) then
        VS.tabulate (n1, fn i => f (VS.sub (v1, i), VS.sub (v2, i)))
      else
        (raise Fail "mapEq")
    end

end
