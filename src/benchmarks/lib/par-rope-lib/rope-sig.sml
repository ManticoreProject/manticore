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

  (* Determines the maximum possible size of a leaf *)
    val maxLeafSize : int

  (** Constant-time operations **)

  (* returns the empty rope *)
    val empty      : unit -> 'a rope
  (* returns true if the given rope is an empty leaf node *)
    val isEmpty    : 'a rope -> bool
  (* returns true if the given rope is represented by a leaf node *)
    val isLeaf     : 'a rope -> bool
  (* returns the size of the given rope *)
    val length     : 'a rope -> int
  (* returns the depth of the given rope *)
    val depth      : 'a rope -> int

  (** Amortized log-time operations; the time is O(n log n) when rebalancing is necessary **)  

  (* append (r1, r2) *)
  (* returns the rope that is the concatenation of r1 and r2 *)
    val append     : 'a rope * 'a rope -> 'a rope
  (* sub (r, i) *)
  (* returns the ith element of the rope r. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
    val sub        : 'a rope * int -> 'a
  (* update (r, i, x) *)
  (* returns the rope r in which the ith element of r is equal to x. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
    val update        : 'a rope * int * 'a -> 'a rope
  (* delete (r, i) *)
  (* returns the rope r in which the ith element has been removed. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
  (* e.g., delete ([344,3,5,8], 2) ==> [344,3,8] *)
    val delete        : 'a rope * int -> 'a rope
    (* take (r, n) *)
  (* returns the first i elements of the rope r. Raises Subscript if i < 0 or i >= length r. *)
    val take       : 'a rope * int -> 'a rope
  (* drop (r, n) *)
  (* returns what is left after dropping the first i elements of the rope. Raises Subscript if i < 0 *)
  (* or i > length r. It holds that append (take(r, i), drop(r, i)) = r when 0 <= i <= length r. *)
    val drop       : 'a rope * int -> 'a rope
  (* splitAt (r, i) *)
  (* returns two ropes r1 and r2 such that r1 contains the first i+1 elements of the r and r2 contains *)
  (* the rest of the elements of r. r1 and r2 have the same order as r *)
  (* If i < 0 or length r <= i, then the Subscript exception is raised *)
    val splitAt    : 'a rope * int -> ('a rope * 'a rope)

  (* concat l *)
  (* returns the rope that is the concatenation of all the ropes in the list l *)
    val concat     : 'a rope list -> 'a rope

  (** Linear-work / log-span operations **)

  (* rev r *)
  (* returns a rope consisting of r's elements in reverse. *)
    val rev        : 'a rope -> 'a rope
  (* map f r *)
  (* applies f to each element of r from left to right, returning the list of results. *)
    val map        : ('a -> 'b) -> 'a rope -> 'b rope
  (* mapPartial f r *)
  (* applies f to each element of r from left to right, returning a rope of results where f *)
  (* was defined. f is not defined for an element of r if f applied to the element returns NONE. *)
    val mapPartial : ('a -> 'b option) -> 'a rope -> 'b rope
  (* filter f r *)
  (* applies f to each element x of r, from left to right, and returns the rope of those x for which *)
  (* f x evaluated to true, in the same order as the occurred in the argument rope. *)
    val filter     : ('a -> bool) -> 'a rope -> 'a rope
  (* partition f r *)
  (* applies f to each element x of r, from left to right, and returns a pair (pos, neg) where pos is *)
  (* the rope of those x for which f x evaluated to true, and neg is the rope of those for which f x *)
  (* evaluated to false. The elements of pos and neg retain the same relative order they possessed in r. *) 
    val partition  : ('a -> bool) -> 'a rope -> ('a rope * 'a rope)
  (* reduce aop z r *)
  (* returns the reduction z aop r_0 aop r_1 ... aop r_n *)
  (* pre: aop is an associative operator *)
    val reduce     : ('a * 'a -> 'a) -> 'a -> 'a rope -> 'a

  (** Speculative operations having worst case log-span time **)

  (* find f r *)
  (* applies f to each element x of the rope r, from left to right, until f x evaluates to true; *)
  (* returns SOME x if such an x exists, otherwise NONE. *)
    val find       : ('a -> bool) -> 'a rope -> 'a option
  (* exists f r *)
  (* applies f to each element x of the rope r, from left to right, until f x evaluates to true; *)
  (* returns true if such an x exists and false otherwise. *)
    val exists     : ('a -> bool) -> 'a rope -> bool
  (* all f r *)
  (* applies f to each element x of the rope r, from left to right, until f x evaluates to false; *)
  (* returns false if such an x exists and true otherwise. Equivalent to not(exists (not o f) r)). *)
    val all        : ('a -> bool) -> 'a rope -> bool

  (** Linear-work operations **)

  (* foldl f z r *)
  (*
     returns
	    f(rn,...,f(r2, f(r1, z))...)
     or z if the list is empty. 
   *)
    val foldl      : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b
  (* foldli f z r *)
  (*
     returns
	    f(n-1, rn,...,f(1, r2, f(0, r1, z))...)
     or z if the list is empty. 
   *)
    val foldli     : (int * 'a * 'b -> 'b) -> 'b -> 'a rope -> 'b
  (* foldr f z r *)
  (*
     returns
	    f(r1, f(r2, ..., f(rn, z)...))
     or b if the list is empty. 
   *)
    val foldr      : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b
  (* foldri f z r *)
  (*
     returns
	    f(0, r1, f(1, r2, ..., f(n-1, rn, z)...))
     or b if the list is empty. 
   *)
    val foldri     : (int * 'a * 'b -> 'b) -> 'b -> 'a rope -> 'b
  (* app f r *)
  (* applies f to the elements of l, from left to right. *)
    val app        : ('a -> unit) -> 'a rope -> unit
  (* appi f r *)
  (* applies f to the elements of l, from left to right. *)
    val appi       : (int * 'a -> unit) -> 'a rope -> unit

  (** Coercions to and from other data structures **)

  (* creates a new rope from a sequence of elements. *)
    val fromSeq    : 'a seq  -> 'a rope
  (* creates a new sequence from a rope of elements *)
    val toSeq      : 'a rope -> 'a seq
  (* creates a new from from a list of elements. *)
    val fromList   : 'a list -> 'a rope
  (* creates a new list from a rope of elements *)
    val toList     : 'a rope -> 'a list
  (* creates a new rope of characters from a string *)
    val fromString : string  -> char rope

  (** Constructors **)

  (* creates a rope containing just x *)
    val singleton  : 'a -> 'a rope
  (* creates a rope of n elements, where the elements are defined in order of increasing index by applying *)
  (* f to the element's index. This is equivalent to the expression: *)
  (*   fromList (List.tabulate (n, f)) *)
    val tabulate   : (int * (int -> 'a)) -> 'a rope

  (** Utilities **)

  (* returns a string displaying the internal structure of the given rope *)
    val toString   : ('a -> string) -> 'a rope -> string

  (* randomRope (f, maxDepth, leafSize) *)
  (* returns a randomly generated rope of a maximum depth and a fixed leaf size. each element *)
  (* is generated from an application of f. *)
    val randomRope : (unit -> 'a) * int * int -> 'a rope

    structure Pair : ROPE_PAIR where type 'a rope = 'a rope
    structure Scan : ROPE_SCAN where type 'a rope = 'a rope
    structure Permute : ROPE_PERMUTE where type 'a rope = 'a rope

  end
