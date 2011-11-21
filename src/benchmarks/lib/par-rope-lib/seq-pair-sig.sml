(* seq-pair-sig.sml
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

signature SEQ_PAIR =
  sig

    exception UnequalLengths

    type 'a seq

  (* leftover (s1, s2) *)
  (* returns the spill-over elements of s1 and s2, or NONE if |s1| = |s2| *)
  (* e.g., leftover ([1,2,3], [4]) ==> SOME (LEFT [2,3]) *)
  (* e.g., leftover ([1,2], [3,4,5,6]) ==> SOME (RIGHT [4,5,6]) *)
  (* e.g., leftover ([1,2], [3,4]) ==> NONE *)
    val leftover : 'a seq * 'b seq -> ('a seq, 'b seq) Either.either option

    val map : ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq

    val mapUntil : int -> (unit -> bool) -> ('a * 'b -> 'c) -> 'a seq * 'b seq -> 
		     ('a seq, 'b seq, 'c seq) ProgressTy.progress2

  end
