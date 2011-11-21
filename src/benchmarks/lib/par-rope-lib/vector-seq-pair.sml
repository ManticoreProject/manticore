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

structure VectorSeqPair : SEQ_PAIR =
  struct

    exception UnequalLengths

    type 'a seq = 'a Vector.vector

    datatype either = datatype Either.either
    datatype progress2 = datatype ProgressTy.progress2

    fun toList v = Vector.foldr (fn (x, ls) => x :: ls) [] v

    val fromList = Vector.fromList

    fun output ls = fromList (List.rev ls)

    fun leftover (s1, s2) =
	let val len1 = Vector.length s1
	    val len2 = Vector.length s2
	    val diff = if len1 > len2 then len1 - len2 else len2 - len1
	in
	    if diff = 0 then NONE
	    else if len1 > len2 then SOME (LEFT (Vector.tabulate (diff, fn i => Vector.sub (s1, len2 + i))))
	    else SOME (RIGHT (Vector.tabulate (diff, fn i => Vector.sub (s2, len1 + i))))
	end

    fun map f (s1, s2) =
	let val len = Int.min (Vector.length s1, Vector.length s2)
	in
	    Vector.tabulate (len, fn i => f (Vector.sub (s1, i), Vector.sub (s2, i)))
	end

    fun mapUntil k cond f (s1, s2) =
	let val len = Int.min (Vector.length s1, Vector.length s2)
	    fun advanceK k' = if k' <= 1 then k else k' - 1
	    fun mp (i, k', prd) =
		if i >= len then COMPLETE2 (output prd)
		else if k' <= 1 andalso cond () then
		    PARTIAL2 (Vector.tabulate (Vector.length s1 - i, fn j => Vector.sub (s1, i + j)),
			      Vector.tabulate (Vector.length s2 - i, fn j => Vector.sub (s2, i + j)),
			      output prd)
		else mp (i + 1, advanceK k', f (Vector.sub (s1, i), Vector.sub (s2, i)) :: prd)
	in
	    mp (0, k, nil)
	end

  end
