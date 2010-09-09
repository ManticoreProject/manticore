(* seq-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generic sequences for constructing ropes.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

signature SEQ = 
  sig

    type 'a seq

    val empty      : 'a seq
    val isEmpty    : 'a seq -> bool
    val length     : 'a seq -> int
    val singleton  : 'a -> 'a seq

    val concat     : 'a seq * 'a seq -> 'a seq
    val sub        : 'a seq * int -> 'a
  (* given a sequence s and an index i, split the sequence into s[0, ..., i] and s[i+1, ..., length s] *)
    val splitAt    : 'a seq * int -> ('a seq * 'a seq)

    val rev        : 'a seq -> 'a seq
    val map        : ('a -> 'b) -> 'a seq -> 'b seq
    val filter     : ('a -> bool) -> 'a seq -> 'a seq
    val foldl      : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b
    val foldr      : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b

    val take       : 'a seq * int -> 'a seq
    val drop       : 'a seq * int -> 'a seq
  (* cut the sequence s into s[0, ..., n-1] and s[n, ..., length s] *)
    val cut        : 'a seq * int -> 'a seq * 'a seq

    val fromList   : 'a list -> 'a seq
    val toList     : 'a seq -> 'a list

  end
