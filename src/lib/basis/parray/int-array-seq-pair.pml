(* int-array-seq-pair.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure IntArraySeqPair = struct

  structure A = IntArray
  structure S = IntArraySeq

  val fail = Fail.fail "IntArraySeqPair"

  fun tabulate (n, f) = 
    if (n=0) then (S.empty, S.empty)
    else let
      val (a0, b0) = f(0)
      val a = A.array (n, a0)
      val b = A.array (n, b0)
      fun lp i = 
        if (i >= n) then 
	  (a, b)
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

  fun mapEq_int (f, s1, s2) = let
    val n1 = S.length s1
    val _ = if (n1 = S.length s2) then () else fail "mapEq_int" "len"
    fun f' i = f (S.sub (s1, i), S.sub (s2, i))
    in
      S.tabulate_int (n1, f')
    end

  fun mapEq_intPair (f, s1, s2) = let
    val n1 = S.length s1
    val _ = if (n1 = S.length s2) then () else fail "mapEq_intPair" "len"
    fun f' i = f (S.sub (s1, i), S.sub (s2, i))
    in
      tabulate (n1, f')
    end

end
