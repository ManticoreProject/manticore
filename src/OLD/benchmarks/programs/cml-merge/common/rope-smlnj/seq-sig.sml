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

    val append     : 'a seq * 'a seq -> 'a seq
    val sub        : 'a seq * int -> 'a

    val rev        : 'a seq -> 'a seq
    val map        : ('a -> 'b) -> 'a seq -> 'b seq
    val mapPartial : ('a -> 'b option) -> 'a seq -> 'b seq
    val filter     : ('a -> bool) -> 'a seq -> 'a seq
    val foldl      : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b
    val foldr      : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b
    val app        : ('a -> unit) -> 'a seq -> unit
    val find       : ('a -> bool) -> 'a seq -> 'a option
    val exists     : ('a -> bool) -> 'a seq -> bool
    val all        : ('a -> bool) -> 'a seq -> bool

    val take       : 'a seq * int -> 'a seq
    val drop       : 'a seq * int -> 'a seq
  (* cut the sequence s into s[0, ..., n-1] and s[n, ..., length s] *)
    val cut        : 'a seq * int -> 'a seq * 'a seq

    val subseq     : 'a seq * int * int -> 'a seq

    val fromList   : 'a list -> 'a seq
    val toList     : 'a seq -> 'a list

  end
