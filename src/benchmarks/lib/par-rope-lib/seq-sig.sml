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

  (* returns the empty sequence *)
    val empty      : unit -> 'a seq
  (* returns true if the given sequence is empty *)
    val isEmpty    : 'a seq -> bool
  (* returns the size of the given sequence *)
    val length     : 'a seq -> int

  (* append (s1, s2) *)
  (* returns the sequence that is the concatenation of s1 and s2 *)
    val append     : 'a seq * 'a seq -> 'a seq
  (* sub (s, i) *)
  (* returns the ith element of the sequence s. If i < 0 or length s <= i, then the Subscript *)
  (* exception is raised. *)
    val sub        : 'a seq * int -> 'a
  (* update (s, i, x) *)
  (* returns the sequence s in which the ith element of s is equal to x. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
    val update        : 'a seq * int * 'a -> 'a seq
  (* delete (s, i) *)
  (* returns the sequence s in which the ith element has been removed. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
  (* e.g., delete ([344,3,5,8], 2) ==> [344,3,8] *)
    val delete        : 'a seq * int -> 'a seq

  (* rev s *)
  (* returns a sequence consisting of s's elements in reverse. *)
    val rev        : 'a seq -> 'a seq
  (* map f s *)
  (* applies f to each element of s from left to right, returning the list of results. *)
    val map        : ('a -> 'b) -> 'a seq -> 'b seq
  (* mapUntil k cond f s *)
  (* returns either the result of mapping f over s or, if cond () returns true the pair (unprd, prd). *)
  (* - unprd records the elements yet to be processed and prd processed ones *)
  (* - k is the number of elements to process before checking the condition *)
  (* pre: k > 0 *)
    val mapUntil : int -> (unit -> bool) -> ('a -> 'b) -> 'a seq -> ('a seq, 'b seq) ProgressTy.progress
  (* mapPartial f s *)
  (* applies f to each element of s from left to right, returning a sequence of results where f *)
  (* was defined. f is not defined for an element of s if f applied to the element returns NONE. *)
    val mapPartial : ('a -> 'b option) -> 'a seq -> 'b seq
  (* filter f s *)
  (* applies f to each element x of s, from left to right, and returns the sequence of those x for which *)
  (* f x evaluated to true, in the same order as the occurred in the argument sequence. *)
    val filter     : ('a -> bool) -> 'a seq -> 'a seq
  (* filterUntil k cond pred s *)
  (* returns either the result of filtering s by pred or, if cond () returns true the pair (unprd, prd) *)
  (* - unprd records the unprocessed elements and prd is the processed ones *)
  (* - k is the number of elements to process before checking the condition *)
  (* pre: k > 0 *)
    val filterUntil : int -> (unit -> bool) -> ('a -> bool) -> 'a seq -> ('a seq, 'a seq) ProgressTy.progress
  (* reduceUntil k aop z s *)
  (* returns either the reduction z aop s_0 aop s_1 ... aop s_n or, if cond () returns true, *)
  (* the pair (unprd, acc) where unprd records the unprocessed elements and acc the result of the reduction at the *)
  (* point of interruption *)
  (* - k is the number of elements to process before checking the condition *)
  (* pre: aop is an associative operator *)
    val reduceUntil : int -> (unit -> bool) -> ('a * 'a -> 'a) -> 'a -> 'a seq -> ('a seq * 'a seq, 'a) ProgressTy.progress
  (* scanl aop z s *)
    val scanl       : ('a * 'a -> 'a) -> 'a -> 'a seq -> ('a seq * 'a)
  (* scanlUntil aop z s *)
    val scanlUntil       : int -> (unit -> bool) -> ('a * 'a -> 'a) -> 'a -> 'a seq -> ('a seq, 'a seq * 'a) ProgressTy.progress
  (* foldl f z s *)
  (*
     returns
	    f(rn,...,f(s2, f(s1, z))...)
     or z if the list is empty. 
   *)
    val foldl      : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b
  (* foldr f z s *)
  (*
     returns
	    f(s1, f(s2, ..., f(rn, z)...))
     or b if the list is empty. 
   *)
    val foldr      : ('a * 'b -> 'b) -> 'b -> 'a seq -> 'b
  (* app f s *)
  (* applies f to the elements of l, from left to right. *)
    val app        : ('a -> unit) -> 'a seq -> unit
  (* find f s *)
  (* applies f to each element x of the sequence s, from left to right, until f x evaluates to true; *)
  (* returns SOME x if such an x exists, otherwise NONE. *)
    val find       : ('a -> bool) -> 'a seq -> 'a option
  (* exists f s *)
  (* applies f to each element x of the sequence s, from left to right, until f x evaluates to true; *)
  (* returns true if such an x exists and false otherwise. *)
    val exists     : ('a -> bool) -> 'a seq -> bool
  (* all f s *)
  (* applies f to each element x of the sequence s, from left to right, until f x evaluates to false; *)
  (* returns false if such an x exists and true otherwise. Equivalent to not(exists (not o f) s)). *)
    val all        : ('a -> bool) -> 'a seq -> bool

  (* take (r, n) *)
  (* returns the first i elements of the sequence s. Raises Subscript if i < 0 or i >= length s. *)
    val take       : 'a seq * int -> 'a seq
  (* drop (r, n) *)
  (* returns what is left after dropping the first i elements of the sequence. Raises Subscript if i < 0 *)
  (* or i > length s. It holds that append (take(r, i), drop(r, i)) = s when 0 <= i <= length s. *)
    val drop       : 'a seq * int -> 'a seq
  (* cut the sequence s into s[0, ..., n-1] and s[n, ..., length s] *)
    val cut        : 'a seq * int -> 'a seq * 'a seq

  (* creates a sequence containing just x *)
    val singleton  : 'a -> 'a seq
  (* creates a sequence of n elements, where the elements are defined in order of increasing index by applying *)
  (* f to the element's index. This is equivalent to the expression: *)
  (*   fromList (List.tabulate (n, f)) *)
    val tabulate   : int * (int -> 'a) -> 'a seq
  (* tabulateUntil k cond (lo, hi, f) *)
    val tabulateUntil   : int -> (unit -> bool) -> int * int * (int -> 'a) -> (unit, 'a seq) ProgressTy.progress

  (* creates a new from from a list of elements. *)
    val fromList   : 'a list -> 'a seq
  (* creates a new list from a sequence of elements *)
    val toList     : 'a seq -> 'a list

    structure Pair : SEQ_PAIR where type 'a seq = 'a seq

  end
