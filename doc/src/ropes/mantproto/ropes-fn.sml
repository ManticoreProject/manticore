(* ropes-fn.sml
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
 * We have based our implementation on the original paper by Boehm et. al.

@article{bap:ropes,
    author = {Hans-J. Boehm and Russ Atkinson and Michael Plass},
    title = {Ropes: an alternative to strings},
    journal = {Software---Practice \& Experience},
    volume = 25,
    number = 12,
    year = 1995,
    issn = {0038-0644},
    pages = {1315--1330},
    publisher = {John Wiley \& Sons, Inc.},
    address = {New York} 
    }

 *)

functor RopesFn (
    structure S : SEQ
    val maxLeafSize     : int
  ) : ROPES = struct

    structure S = S
    type 'a seq = 'a S.seq

  (* ***** UTILITIES ***** *)

  (* log : real -> (real -> real) *)
    fun log base x = Math.ln x / Math.ln base

  (* ceilingLg : int -> int *)
  (* The ceiling of the log_2 of the input. *)
    val ceilingLg = ceil o log 2.0 o real

  (* fib : int -> int *)
  (* Compute the nth Fibonacci number, where *)
  (*   fib 0 is 0, fib 1 is 1, fib 2 is 1, etc. *)
  (* Returns 0 for negative args, so be careful. *)
    fun fib n = let
      fun ff args =
       (case args
	  of (0, u, p) => u
	   | (n, u, p) => ff (n-1, u+p, u)
          (* end case *))
      in
        if n < 1 then 0
	else if n = 0 then 1
	else ff (n, 0, 1)
      end
  
  (* ***** ROPES ***** *)

  (* The rope datatype and some basic operations. *)

    datatype 'a rope
      = CAT of (int *     (* depth *)
		int *     (* length *)
		'a rope * (* left subtree *)
		'a rope   (* right subtree *))
      | LEAF of (int *    (* length *)
		 'a seq   (* sequence *))

  (* maxLeafSize : int *)
    val maxLeafSize = maxLeafSize

  (* empty : 'a rope *)
    val empty = LEAF(0, S.empty)

  (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r = let
      fun copies thing n = List.tabulate (n, fn _ => thing)
      val rootString = "C<"
      val spaces = copies " "
      val indenter = String.concat (spaces (String.size rootString))
      val indent = List.map (fn s => indenter ^ s) 
      fun build r =
       (case r
	 of LEAF (_, xs) => let 
              fun b args = 
               (case args
	         of (nil, acc) => "]" :: acc
		  | (x::nil, acc) => b (nil, show x :: acc)
		  | (x::xs, acc) => b (xs, "," :: show x ::acc)
	         (* end case *))
              in
		(String.concat(List.rev(b (S.toList xs, ("["::nil))))) :: nil
              end
	  | CAT (_, _, r1, r2) => let 
              val ss1 = build r1
	      val ss2 = build r2
	      in
	        (indent ss1) @ (rootString :: (indent ss2))
	      end	
         (* end case *))
      in
        String.concatWith "\n" (build r @ ("\n"::nil))
      end

  (* isLeaf : 'a rope -> bool *)
    fun isLeaf r = 
     (case r
        of LEAF _ => true
	 | CAT _ => false
        (* end case *)) 

  (* isBalanced : 'a rope -> bool *)
  (* balancing condition for ropes *)
  (* The max depth here is given in Boehm et al. 95. *)
    fun isBalanced r = 
     (case r
        of LEAF _ => true
	 | CAT (depth, len, _, _) => (depth <= ceilingLg len + 2)
        (* end case *))

  (* singleton : 'a -> 'a rope *)
    fun singleton x = LEAF (1, S.singleton x)

  (* isEmpty : 'a rope -> bool *)
    fun isEmpty r = 
     (case r
        of LEAF (0, _) => true
	 | CAT (_, 0, _, _) => true
	 | _ => false
        (* end case *))

  (* length : 'a rope -> int *)
    fun length r = 
     (case r
        of LEAF (len, s) => len
	 | CAT(_, len, r1, r2) => len
        (* end case *))

  (* depth : 'a rope -> int *)
    fun depth r = 
     (case r
        of LEAF (_, _) => 0
	 | CAT(depth, _, _, _) => depth
        (* end case *))

  (* inBounds : 'a rope * int -> bool *)
  (* Is the given int a valid index of the rope at hand? *)
    fun inBounds (r, i) = i < length r andalso i >= 0

  (* subInBounds : 'a rope * int -> 'a *)
  (* pre: inBounds (r, i) *)
    fun subInBounds (r, i) = 
     (case r
        of LEAF (_, s) => S.sub(s, i)
	 | CAT (depth, len, r1, r2) =>
	     if i < length r1 then 
               subInBounds(r1, i)
	     else 
               subInBounds(r2, i - length r1)
        (* end case *))

  (* sub : 'a rope * int -> 'a *)
  (* subscript; returns r[i] *)
    fun sub (r, i) = 
      if inBounds (r, i) 
      then subInBounds(r, i)
      else raise Fail "subscript out of bounds"

  (* ***** BALANCING ***** *)

  (* We follow the rope balancing algorithm given in Boehm et al. 1995 *)

  (* That algorithm requires a data structure we call a "balancer". *)
    type 'a balancer = (int * int * 'a rope option) list

  (* at each position a balancer contains
   *   - an inclusive lower bound on the rope length that may inhabit the 
   *      location (the inclusive lower bound is fib(n+1) where n is the index 
   *      of that spot),
   *   - an exclusive upper bound, and
   *   - some rope or none
   *)

  (* balancerLen : int -> int *)
  (* the index of the smallest fibonacci number greater than len, *)
  (* where F_0 = 0, F_1 = 1, etc. *)
  (*   e.g., balancerLen 34 = 8 *)
    fun balancerLen len = let
	  fun lp n =
	        if fib n > len
		   then n
		else lp (n + 1)
          in
	    lp 0 - 2
	  end

  (* mkInitialBalancer : int -> 'a balancer *)
  (* takes a rope length, and returns a rope balancer *)
    fun mkInitialBalancer len = let
      val blen = balancerLen len
      fun initEntry n = (fib (n+2), fib (n+3), NONE)
      in
        List.tabulate (blen, initEntry)
      end

  (* leftmostLeaf : 'a rope -> 'a rope *)
    fun leftmostLeaf r = 
     (case r
        of LEAF _ => r
	 | CAT (_, _, rL, _) => leftmostLeaf rL) 

  (* rightmostLeaf : 'a rope -> 'a rope *)
    fun rightmostLeaf r =
     (case r 
        of LEAF _ => r
	 | CAT (_, _, _, rR) => rightmostLeaf rR)

  (* attachLeft : 'a seq * 'a rope -> 'a rope *)
  (* pre: the rightmost leaf of the rope can accommodate the sequence *)
    fun attachLeft (s, r) = let
      val slen = S.length s
      fun go r =
       (case r
          of CAT (d, len, r1, r2) => CAT (d, len+slen, go r1, r2)
	   | LEAF (len, s') => LEAF (len+slen, S.concat (s, s'))
          (* end case *))
      in
	go r
      end

  (* attachRight : 'a rope * 'a seq -> 'a rope *)
  (* pre: the leftmost leaf of the rope can accommodate the sequence *)
    fun attachRight (r, s) = let
      val slen = S.length s
      fun go r =
       (case r
	  of CAT (d, len, r1, r2) => CAT (d, len+slen, r1, go r2)
	   | LEAF (len, s') => LEAF (len+slen, S.concat (s', s))
          (* end case *))
      in
        go r
      end

  (* concatWithoutBalancing : 'a rope * 'a rope -> 'a rope *)
  (* Concatenates two ropes without balancing. *)
  (* That is, if the resulting rope is unbalanced, so be it. *)
  (* Concatenates naturally, but handles the following special cases: *)
  (* - if either rope is empty, the other rope is returned as-is *)
  (* - if the ropes are both leaves, and they can be fit in a single leaf, they are *)
  (* - if the ropes are both leaves, and they can't be fit in a single leaf, they're *)
  (*     packed to the left in a pair of leaves *)
  (* - if the left rope is a cat and the right is a leaf, and the right leaf can be *)
  (*     packed into the rightmost leaf of the left, it is *)
  (* - symm. case to previous *)
    fun concatWithoutBalancing (r1, r2) =
     (if isEmpty r1 then 
        r2
      else if isEmpty r2 then
	r1
      else (case (r1, r2)
        of (LEAF (len1, s1), LEAF (len2, s2)) =>
	     if (len1 + len2) <= maxLeafSize
	     then LEAF (len1 + len2, S.concat (s1, s2))
	     else let
	       val df  = maxLeafSize - S.length s1
	       val s1' = S.concat (s1, S.take (s2, df))
	       val s2' = S.drop (s2, df)
	       val lf1 = LEAF (maxLeafSize, s1')
	       val lf2 = LEAF (len2 - df, s2')
	       in
                 CAT (1, len1 + len2, lf1, lf2)
               end
	 | (CAT (d, len1, r1, r2), LEAF (len2, s2)) => let
	     val c = CAT (d, len1, r1, r2)
	     val leaf = LEAF (len2, s2)
	     val rmost = rightmostLeaf r2
	     val n = length rmost + len2
	     in
	       if n <= maxLeafSize 
	       then CAT (d, len1 + len2, r1, attachRight (r2, s2))
	       else CAT (d+1, len1 + len2, c, leaf)
	     end
	 | (LEAF (len1, s1), CAT (d, len2, r1, r2)) => let
	     val leaf = LEAF (len1, s1)
	     val c = CAT (d, len2, r1, r2)
	     val lmost = leftmostLeaf r1
	     val n = len1 + length lmost
	     in
	       if n <= maxLeafSize
	       then CAT (d, len1 + len2, attachLeft (s1, r1), r2)
	       else CAT (d+1, len1 + len2, leaf, c)
	     end 
	 | _ => let
             val newDepth = 1 + Int.max (depth r1, depth r2)
	     val newLen = length r1 + length r2
	     in
	       CAT (newDepth, newLen, r1, r2)
	     end
	   (* end case *))
     (* end if *))
 
  (* balToRope : 'a balancer -> 'a rope *)              
  (* Concatenate all ropes in the balancer into one balanced rope. *)
    fun balToRope balancer = let
      fun f (b, acc) = 
       (case b
	  of (_, _, NONE) => acc
	   | (_, _, SOME r) => concatWithoutBalancing (r, acc)
          (* end case *))
      in
        List.foldl f empty balancer
      end

  (* insert : 'a rope * 'a balancer -> 'a balancer *)
  (* Insert a rope into a balancer. *)
  (* invariant: the length of the rope at position i is in its interval, that is, *)
  (*   greater than or equal to the lower bound, and less that the upper bound. *)
  (* See Boehm et al. '95 for details. *)
    fun insert (r, balancer) = 
     (case balancer
        of nil => (* this case should never be reached *)
	          (raise Fail "BUG: empty balancer")
	 | (lb, ub, NONE) :: nil =>
             if length r >= lb andalso length r < ub then
               (lb, ub, SOME r)::nil
	     else 
               (raise Fail "BUG: typing to fit a rope of incompatible size")
	 | (lb, ub, NONE) :: t => 
	     if length r >= lb andalso length r < ub then 
               (lb, ub, SOME r) :: t
	     else 
               (lb, ub, NONE) :: insert (r, t)
	 | (lb, ub, SOME r') :: t =>
             insert (concatWithoutBalancing (r', r), (lb, ub, NONE) :: t)
        (* end case *))

  (* leaves : 'a rope -> 'a rope list *)
  (* takes a rope and returns the list of leaves in left-to-right order *)
    fun leaves r = 
     (case r
        of LEAF _ => r :: nil
	 | CAT (_, _, r1, r2) => leaves r1 @ leaves r2
        (* end case *))

  (* balance : 'a rope -> 'a rope *)
  (* Balance a rope to within 2 of ideal depth. *)
  (* This operation is O(n*log n) in the number of leaves *)
    fun balance r = balToRope (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balanceIfNecessary : 'a rope -> 'a rope *)
  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary r = if isBalanced r then r else balance r

  (* ***** ROPE CONSTRUCTION ***** *)

  (* concatWithBalancing : 'a rope * 'a rope -> 'a rope *)
  (* concatenates two ropes (with balancing) *)
    fun concatWithBalancing (r1, r2) = balanceIfNecessary(concatWithoutBalancing(r1, r2))

  (* concat : 'a rope -> 'a rope *)
    val concat = concatWithBalancing

  (* toSeq : 'a rope -> 'a seq *)
  (* return the fringe of the data at the leaves of a rope as a sequence *)
    fun toSeq r = 
     (case r
        of LEAF(_, s) => s
	 | CAT(_, _, r1, r2) => S.concat (toSeq r1, toSeq r2)
        (* end case *))

  (* fromSeq : 'a seq -> 'a rope *)
  (* given a sequence, construct a balanced rope *)
  (* FIXME This can be done with a different, nice algorithm... - ams *)
    fun fromSeq xs = let
      val len = S.length xs
      in
	if len <= maxLeafSize
	   then LEAF (len, xs)
	else let
          val m = len div 2
          val (xs1, xs2) = (S.take(xs, m), S.drop(xs, m))
	  in
	    concatWithoutBalancing (fromSeq xs1, fromSeq xs2)
	  end
      end

  (* ***** ROPE DECONSTRUCTION ***** *)

  (* splitAtWithoutBalancing : 'a rope * int -> 'a rope * 'a rope *)
  (* pre: inBounds(r, i) *)
    fun splitAtWithoutBalancing (r, i) = 
     (case r
        of LEAF (len, s) => let
	     val (s1, s2) = S.splitAt(s, i)
	     in
	       (LEAF (S.length s1, s1), LEAF (S.length s2, s2))
	     end
	 | CAT (depth, len, r1, r2) =>
	     if i = length r1 - 1 then
               (r1, r2)
	     else if i < length r1 then let
               val (r11, r12) = splitAtWithoutBalancing(r1, i)
               in
                 (r11, concatWithoutBalancing(r12, r2))
               end
	     else let
               val (r21, r22) = splitAtWithoutBalancing(r2, i - length r1)
               in
                 (concatWithoutBalancing(r1, r21), r22)
               end
        (* end case *))

  (* splitAtWithBalancing : 'a rope * int -> 'a rope * 'a rope *)
  (* pre: inBounds (r, i) *)
    fun splitAtWithBalancing (r, i) = let
      val (r1, r2) = splitAtWithoutBalancing (r, i)
      in
        (balanceIfNecessary r1, balanceIfNecessary r2)
      end

  (* splitAt : 'a rope * int -> 'a rope * 'a rope *)
  (* split a rope in two at index i. (r[0, ..., i], r[i+1, ..., |r|-1]) *)
    fun splitAt (r, i) =
      if inBounds(r, i)
      then splitAtWithBalancing(r, i)
      else raise Fail "subscript out of bounds for splitAt"

  (* naturalSplit : 'a rope -> 'a rope * 'a rope *)
  (* If a rope is a CAT, splits it at the root. *)
  (* If a rope is a LEAF, splits it into two leaves of roughly equal size. *)
    fun naturalSplit r =
     (case r
        of LEAF (len, s) => let
             val len' = len div 2
	     val (s1, s2) = S.cut (s, len')
             in
               (LEAF (len', s1), LEAF (len - len', s2))
	     end
	 | CAT (_, _, r1, r2) => (r1, r2)
        (* end case *))

  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r] *)
    fun cut (r, n) =
	  if n = 0
	     then (empty, r)
	  else splitAt(r, n - 1)

    fun take (r, n) = #1(cut(r, n))

    fun drop (r, n) = #2(cut(r, n))

  (* rev : 'a rope -> 'a rope *)
  (* pre  : the input is balanced *)
  (* post : the output is balanced *)
    fun rev r = 
     (case r
        of LEAF (len, s) => LEAF (len, S.rev s)
	 | CAT (dpt, len, r1, r2) => let
             val r2' = rev r2
             in
               CAT (dpt, len, r2', rev r1)
             end 
        (* end case *))

  (* map : ('a -> 'b) -> 'a rope -> 'b rope *)
  (* post : the output has the same shape as the input *)
    fun map f rope = let
      fun m r =
       (case r
          of LEAF (len, s) => LEAF (len, S.map f s)
	   | CAT (dpt, len, r1, r2) => let
               val r2' = m r2
               in
                 CAT (dpt, len, m r1, r2')
	       end
          (* end case *))
      in
        m rope
      end          

  (* filter : ('a -> bool) -> 'a rope -> 'a rope *)
  (* post: the output is balanced *)
  (* Strategy: First, filter all the leaves without balancing. *)
  (*           Then balance the whole thing if needed. *)
    fun filter pred rope = let
      fun f r =
       (case r
	  of LEAF (len, s) => let
               val s' = S.filter pred s
               in
                 LEAF (S.length s', s')
	       end
	   | CAT (_, _, r1, r2) => let
               val r2' = f r2
               in
                 concatWithoutBalancing (f r1, r2')
               end
          (* end case *))
      in
        balanceIfNecessary (f rope)
      end

  (* foldl : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b *)
    fun foldl f id r = (
	  case r
	   of LEAF (_, s) => S.foldl f id s
	    | CAT (_, _, r1, r2) => foldl f (foldl f id r1) r2
          (* end case *))

  (* foldr : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b *)
    fun foldr f id r = (
	  case r
	   of LEAF (_, s) => S.foldr f id s
	    | CAT (_, _, r1, r2) => foldr f (foldr f id r2) r1
          (* end case *))

  end
