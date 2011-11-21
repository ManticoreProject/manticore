(* rope-pair-sig.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations for pairs of ropes.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

signature ROPE_PAIR =
  sig

    exception UnequalLengths

    type 'a rope

    val zipEq : 'a rope * 'b rope -> ('a * 'b) rope
    val mapEq : ('a * 'b -> 'c) -> 'a rope * 'b rope -> 'c rope

  end
