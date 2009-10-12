
(* rope-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Ropes for Standard ML.
 * 
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 * We have based our implementation on the paper "Ropes: An Alternative to Strings" 
 * by Boehm et al. (1995). See rope-fn.sml for more detailed commentary.
 *)

signature ROPE = 
  sig

    structure S : SEQ

    type 'a rope
    type 'a seq = 'a S.seq

    val maxLeafSize : int

  (* constant-time operations *)
    val empty      : 'a rope
    val isEmpty    : 'a rope -> bool
    val isLeaf     : 'a rope -> bool
    val length     : 'a rope -> int
    val depth      : 'a rope -> int

  (* amortized log-time operations; the time is O(n log n) when rebalancing is necessary *)  
    val append     : 'a rope * 'a rope -> 'a rope
    val sub        : 'a rope * int -> 'a
    val subrope    : 'a rope * int * int -> 'a rope     (* for i, j, rope from i to i+j-1 *)
    val take       : 'a rope * int -> 'a rope
    val drop       : 'a rope * int -> 'a rope
    val splitAt    : 'a rope * int -> ('a rope * 'a rope)
    val cut        : 'a rope * int -> 'a rope * 'a rope (* cut r into [0..n-1] and [n..length r] *)

  (* linear-time operations *)
    val rev        : 'a rope -> 'a rope
    val map        : ('a -> 'b) -> 'a rope -> 'b rope
    val mapPartial : ('a -> 'b option) -> 'a rope -> 'b rope
    val foldl      : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b
    val foldr      : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b
    val filter     : ('a -> bool) -> 'a rope -> 'a rope
    val partition  : ('a -> bool) -> 'a rope -> ('a rope * 'a rope)
    val app        : ('a -> unit) -> 'a rope -> unit
    val find       : ('a -> bool) -> 'a rope -> 'a option
    val exists     : ('a -> bool) -> 'a rope -> bool
    val all        : ('a -> bool) -> 'a rope -> bool

  (* coercions to and from other data structures *)
    val fromSeq    : 'a seq  -> 'a rope
    val toSeq      : 'a rope -> 'a seq
    val fromList   : 'a list -> 'a rope
    val toList     : 'a rope -> 'a list
    val fromString : string  -> char rope

  (* constructors *)
    val singleton  : 'a -> 'a rope
    val tabulate   : (int * (int -> 'a)) -> 'a rope

  (* utilities *)
    val toString   : ('a -> string) -> 'a rope -> string
    
  end
