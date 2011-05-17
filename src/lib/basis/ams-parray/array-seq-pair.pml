(* array-seq.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure ArraySeqPair = struct

  structure A = Array
  structure S = ArraySeq

  fun tabulate (n, f) = 
    if (n=0) then (S.empty, S.empty)
    else let
      val (a0, b0) = f(0)
      val a = A.array (n, a0)
      val b = A.array (n, b0)
      fun lp i = 
        if (i >= n) then 
	  (S.NonEmpty a, S.NonEmpty b)
	else let
          val (ai, bi) = f(i)
          val _ = A.update (a, i, ai)
	  val _ = A.update (b, i, bi)
          in
            lp (i+1)
	  end
      in
        lp 1
      end

end
