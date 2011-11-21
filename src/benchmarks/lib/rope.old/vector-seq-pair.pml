(* vector-seq-pair.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

(* vector-seq-pair.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations for pairs of sequences.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure VectorSeqPair =
  struct

    type 'a seq = 'a Vector.vector

    datatype either = datatype Either.either
    datatype progress2 = datatype ProgressTy.progress2

    fun toList v = VectorSeq.foldr (fn (x, ls) => x :: ls) [] v

    val fromList = VectorSeq.fromList

    fun output ls = fromList (List.rev ls)

    fun leftover (s1, s2) =
	let val len1 = VectorSeq.length s1
	    val len2 = VectorSeq.length s2
	    val diff = if len1 > len2 then len1 - len2 else len2 - len1
	in
	    if diff = 0 then NONE
	    else if len1 > len2 then SOME (LEFT (VectorSeq.tabulate (diff, fn i => VectorSeq.sub (s1, len2 + i))))
	    else SOME (RIGHT (VectorSeq.tabulate (diff, fn i => VectorSeq.sub (s2, len1 + i))))
	end

    fun map f (s1, s2) =
	let val len = Int.min (VectorSeq.length s1, VectorSeq.length s2)
	in
	    VectorSeq.tabulate (len, fn i => f (VectorSeq.sub (s1, i), VectorSeq.sub (s2, i)))
	end

    fun mapUntil k cond f (s1, s2) =
	let val len = Int.min (VectorSeq.length s1, VectorSeq.length s2)
	    fun advanceK k' = if k' <= 1 then k else k' - 1
	    fun mp (i, k', prd) =
		if i >= len then COMPLETE2 (output prd)
		else if k' <= 1 andalso cond () then
		    PARTIAL2 (VectorSeq.tabulate (VectorSeq.length s1 - i, fn j => VectorSeq.sub (s1, i + j)),
			      VectorSeq.tabulate (VectorSeq.length s2 - i, fn j => VectorSeq.sub (s2, i + j)),
			      output prd)
		else mp (i + 1, advanceK k', f (VectorSeq.sub (s1, i), VectorSeq.sub (s2, i)) :: prd)
	in
	    mp (0, k, nil)
	end

  end
