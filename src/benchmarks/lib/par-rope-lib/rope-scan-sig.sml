(* rope-scan-sig.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

signature ROPE_SCAN =
  sig

    type 'a rope

    val scanl : ('a * 'a -> 'a) -> 'a -> 'a rope -> 'a rope

  end 
