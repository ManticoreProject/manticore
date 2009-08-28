(* rope-fn.sml
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
 * by Boehm et al., 1995. See the bib entry at bottom of this file.
 *
 * The main differences between the original paper and the present code are these:
 *
 * - the original work proposes ropes as an alternative specifically to strings,
 *   with character data at the leaves; ours are generalized, polymorphic ropes
 *
 * - the original work mentions a lazy leaf variant which holds not actual data, but 
 *   a function for computing data on demanded, for use in, among other things, creating 
 *   ropes to represent files without actually reading whole files at a time; we provide 
 *   no such variant 
 *
 * - what they refer to in the paper as "concatenation" of ropes we refer to, 
 *   in function names, as "appending"; this is for consistency with the names of 
 *   similar operations in the ML Basis Library
 *
 * - we provide a reasonably standard set of common Basis Library operations, like
 *   map, mapPartial, find, and so on
 *)

functor RopeFn (

    structure S : SEQ
    val maxLeafSize : int

  ) : ROPE = struct

    structure S = S
    type 'a seq = 'a S.seq

  (* ***** UTILITIES ***** *)

    fun println s = (print s; print "\n")

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
        if n <= 0 then 0
	else if n = 1 then 1
	else ff (n, 0, 1)
      end
  
  (* split : 'a list * int -> 'a list * 'a list *)
  (* Split the list into two pieces. *)
  (* This function is basically take and drop at the same time. *)
  (* It doesn't raise an exn if there aren't enough elements. *)
  (* ex: split ([1,2,3], 0) => ([],[1,2,3]) *)
  (* ex: split ([1,2,3], 1) => ([1],[2,3])  *)
  (* ex: split ([1,2,3], 2) => ([1,2],[3])  *)
  (* ex: split ([1,2,3], 4) => ([1,2,3],[]) *)
    fun split (xs : 'a list, n : int) : ('a list * 'a list) = let
      fun loop (n, taken, []) = (List.rev taken, [])
	| loop (n, taken, L as h::t) = 
            if n = 0 then
              (List.rev taken, L)
            else
              loop (n-1, h::taken, t)
      in
        if n <= 0 
	then ([], xs)
	else loop (n, [], xs)
      end
         
  (* chop : 'a list * int -> 'a list list *)
  (* Chop the list into pieces of the appropriate size. *)
  (* ex: chop ([1,2,3,4], 1) => [[1],[2],[3],[4]] *)
  (* ex: chop ([1,2,3,4], 2) => [[1,2],[3,4]]     *)
    fun chop (xs, n) = let
      fun loop [] = []
	| loop xs = 
            if List.length xs <= n then
              [xs]
	    else let
              val (t, d) = split (xs, n)
              in
                t :: loop d
              end
      in
	loop xs
      end

  (* ***** ROPES ***** *)

  (* The rope datatype and some basic operations. *)

    datatype 'a rope
      = Cat of (int *     (* depth *)
		int *     (* length *)
		'a rope * (* left subtree *)
		'a rope   (* right subtree *))
      | Leaf of 'a seq    (* sequence *)

  (* maxLeafSize : int *)
    val maxLeafSize = maxLeafSize

  (* mkLeaf : 'a seq -> 'a rope *)
  (* this constructor maintains the invariant that the leaf size must be 
   * <= to the constant maxLeafSize *)
    fun mkLeaf (seq : 'a seq) : 'a rope = 
	  if S.length seq > maxLeafSize 
	    then raise Fail "RopeFn.mkLeaf: bogus leaf size"
	  else Leaf seq

    val empty : 'a rope = Leaf S.empty

  (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r = let
      fun copies thing n = List.tabulate (n, fn _ => thing)
      val rootString = "C<"
      val spaces = copies " "
      val indenter = String.concat (spaces (String.size rootString))
      val indent = List.map (fn s => indenter ^ s) 
      fun build r =
       (case r
	 of Leaf xs => let 
              fun b args = 
               (case args
	         of (nil, acc) => "]" :: acc
		  | (x::nil, acc) => b (nil, show x :: acc)
		  | (x::xs, acc) => b (xs, "," :: show x ::acc)
	         (* end case *))
              in
		(String.concat(List.rev(b (S.toList xs, ("["::nil))))) :: nil
              end
	  | Cat (_, _, r1, r2) => let 
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
    fun isLeaf (Leaf _) = true
      | isLeaf _ = false

  (* isBalanced : 'a rope -> bool *)
  (* balancing condition for ropes *)
  (* The max depth here is given in \cite{bap:ropes}. *)
    fun isBalanced (Leaf _) = true
      | isBalanced (Cat (depth, len, _, _)) = (depth <= ((ceilingLg len) + 2))

  (* singleton : 'a -> 'a rope *)
    fun singleton x = Leaf (S.singleton x)

  (* isEmpty : 'a rope -> bool *)
    fun isEmpty (Leaf seq) = (S.length seq = 0)
      | isEmpty (Cat (_, len, _, _)) = (len = 0)

  (* length : 'a rope -> int *)
    fun length (Leaf seq) = S.length seq
      | length (Cat (_, len, _, _)) = len

  (* depth : 'a rope -> int *)
    fun depth (Leaf _) = 0
      | depth (Cat (d, _, _, _)) = d

  (* inBounds : 'a rope * int -> bool *)
  (* Is the given int a valid index of the given rope? *)
    fun inBounds (r, i) = (i < length r) andalso (i >= 0)

  (* subInBounds : 'a rope * int -> 'a *)
  (* pre: inBounds (r, i) *)
    fun subInBounds (Leaf s, i) = S.sub (s, i)
      | subInBounds (Cat (_, len, r1, r2), i) = 
         (if i < length r1 then
            subInBounds (r1, i)
	  else
            subInBounds (r2, i - length r1))

  (* sub : 'a rope * int -> 'a *)
  (* subscript; returns r[i] *)
    fun sub (r, i) = 
      if inBounds (r, i) 
      then subInBounds (r, i)
      else raise Subscript

  (* ***** BALANCING ***** *)

  (* We follow the rope balancing algorithm given in \cite{bap:ropes}. *)

  (* The algorithm requires a data structure we call a "balancer".
   * at each position a balancer contains
   *   - an inclusive lower bound on the rope length that may inhabit the 
   *      location (the inclusive lower bound is fib(n+1) where n is the index 
   *      of that spot),
   *   - an exclusive upper bound, and
   *   - some rope or none
   *)
    type 'a balancer = (int * int * 'a rope option) list

  (* balancerLen : int -> int *)
  (* FIXME comment *)
  (* the index of the smallest Fibonacci number greater than len, *)
  (* where F_0 = 0, F_1 = 1, etc. *)
  (* ex: balancerLen 34 = 8 *)
    fun balancerLen len = let
      fun lp n =
        if fib n > len
	then n
	else lp (n + 1)
      in
	(lp 0) - 2
      end

  (* mkInitialBalancer : int -> 'a balancer *)
  (* takes a rope length, and returns a rope balancer *)
    fun mkInitialBalancer (len : int) : 'a balancer = let
      val blen = balancerLen len
      fun initEntry n = (fib (n+2), fib (n+3), NONE)
      in
        List.tabulate (blen, initEntry)
      end

  (* leftmostLeaf : 'a rope -> 'a rope *)
    fun leftmostLeaf (r as Leaf _) = r
      | leftmostLeaf (Cat (_, _, rL, _)) = leftmostLeaf rL

  (* rightmostLeaf : 'a rope -> 'a rope *)
    fun rightmostLeaf (r as Leaf _) = r
      | rightmostLeaf (Cat (_, _, _, rR)) = rightmostLeaf rR

  (* absorbLeft : 'a seq * 'a rope -> 'a rope *)
  (* absorb the given sequence in the rightmost leaf of the rope *)
  (* pre: the rightmost leaf of the rope can accommodate the sequence *)
    fun absorbLeft (s, r) = let
      val slen = S.length s
      fun build (Cat (d, len, r1, r2)) = Cat (d, len+slen, build r1, r2)
	| build (Leaf s') = Leaf (S.append (s, s'))
      in
        build r
      end

  (* absorbRight : 'a rope * 'a seq -> 'a rope *)
  (* absorb the given sequence in the leftmost leaf of the rope *)
  (* pre: the leftmost leaf of the rope can accommodate the sequence *)
    fun absorbRight (r, s) = let
      val slen = S.length s
      fun build (Cat (d, len, r1, r2)) = Cat (d, len+slen, r1, build r2)
	| build (Leaf s') = Leaf (S.append (s', s))
      in
        build r
      end

  (* appendWithoutBalancing : 'a rope * 'a rope -> 'a rope *)
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
    fun appendWithoutBalancing (r1 : 'a rope, r2 : 'a rope) : 'a rope =
     (if isEmpty r1 then 
        r2
      else if isEmpty r2 then
	r1
      else (case (r1, r2)
        of (Leaf s1, Leaf s2) =>
	     if (S.length s1 + S.length s2) <= maxLeafSize
	     then mkLeaf (S.append (s1, s2))
	     else let
	       val df  = maxLeafSize - S.length s1
	       val s1' = S.append (s1, S.take (s2, df))
	       val s2' = S.drop (s2, df)
	       in
                 Cat (1, S.length s1 + S.length s2, mkLeaf s1', mkLeaf s2')
               end
	 | (Cat (d, len1, r1, r2), Leaf s2) => let
	     val c = Cat (d, len1, r1, r2)
	     val leaf = mkLeaf s2
	     val rmost = rightmostLeaf r2
	     val n = length rmost + S.length s2
	     in
	       if n <= maxLeafSize 
	       then Cat (d, len1 + S.length s2, r1, absorbRight (r2, s2))
	       else Cat (d+1, len1 + S.length s2, c, leaf)
	     end
	 | (Leaf s1, Cat (d, len2, r1, r2)) => let
	     val leaf = mkLeaf s1
	     val c = Cat (d, len2, r1, r2)
	     val lmost = leftmostLeaf r1
	     val n = S.length s1 + length lmost
	     in
	       if n <= maxLeafSize
	       then Cat (d, S.length s1 + len2, absorbLeft (s1, r1), r2)
	       else Cat (d+1, S.length s1 + len2, leaf, c)
	     end 
	 | _ => let
             val newDepth = 1 + Int.max (depth r1, depth r2)
	     val newLen = length r1 + length r2
	     in
	       Cat (newDepth, newLen, r1, r2)
	     end
	   (* end case *))
     (* end if *))
 
  (* balToRope : 'a balancer -> 'a rope *)              
  (* Concatenate all ropes in the balancer into one balanced rope. *)
    fun balToRope balancer = let
      fun f (b, acc) = 
       (case b
	  of (_, _, NONE) => acc
	   | (_, _, SOME r) => appendWithoutBalancing (r, acc)
          (* end case *))
      in
        List.foldl f (mkLeaf S.empty) balancer
      end

  (* insert : 'a rope * 'a balancer -> 'a balancer *)
  (* Insert a rope into a balancer. *)
  (* invariant: the length of the rope at position i is in its interval, that is, *)
  (*   greater than or equal to the lower bound, and less that the upper bound. *)
  (* See \cite{bap:ropes} for details. *)
    fun insert (r : 'a rope, balancer : 'a balancer) : 'a balancer = 
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
             insert (appendWithoutBalancing (r', r), (lb, ub, NONE) :: t)
        (* end case *))

  (* leaves : 'a rope -> 'a rope list *)
  (* takes a rope and returns the list of leaves in left-to-right order *)
    fun leaves (r as Leaf _) = [r]
      | leaves (Cat (_, _, r1, r2)) = leaves r1 @ leaves r2

  (* balance : 'a rope -> 'a rope *)
  (* Balance a rope to within 2 of ideal depth. *)
  (* This operation is O(n*log n) in the number of leaves *)
    fun balance r = balToRope (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balanceIfNecessary : 'a rope -> 'a rope *)
  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary r = if isBalanced r then r else balance r

  (* ***** ROPE CONSTRUCTION ***** *)

  (* appendWithBalancing : 'a rope * 'a rope -> 'a rope *)
  (* concatenates two ropes (with balancing) *)
    fun appendWithBalancing (r1, r2) = balanceIfNecessary (appendWithoutBalancing (r1, r2))

  (* append : 'a rope -> 'a rope *)
    val append = appendWithBalancing

  (* toSeq : 'a rope -> 'a seq *)
  (* return the fringe of the data at the leaves of a rope as a sequence *)
    fun toSeq (Leaf s) = s
      | toSeq (Cat (_, _, rL, rR)) = S.append (toSeq rL, toSeq rR)

  (* leafFromList : 'a list -> 'a rope *)
    fun leafFromList xs = let
      val n = List.length xs
      in
        if n <= maxLeafSize 
	then mkLeaf (S.fromList xs)
        else raise Fail "too big"
      end

  (* appendPairs : 'a rope list -> 'a rope list *)
  (* Append ropes in a list pairwise. *)
  (* ex: appendPairs [r0,r1,r2] => [Cat(r0,r1),r2] *)
  (* ex: appendPairs [r0,r1,r2,r3] => [Cat(r0,r1),Cat(r2,r3)] *)
    fun appendPairs [] = []
      | appendPairs [r] = [r]
      | appendPairs (r0::r1::rs) = 
          appendWithoutBalancing (r0, r1) :: appendPairs rs

  (* fromList : 'a list -> 'a rope *)
  (* Given a list, construct a balanced rope. *)
  (* Balancing is unnecessary, since the algorithm ensures balance. *)
  (* The leaves will be packed to the left.  *)
    fun fromList (xs : 'a list) : 'a rope = let
      val ldata = chop (xs, maxLeafSize)
      val leaves = List.map leafFromList ldata
      fun build [] = mkLeaf S.empty
	| build [r] = r
	| build rs = build (appendPairs rs)
      in
        build leaves
      end

  (* fromSeq : 'a seq -> 'a rope *)
    fun fromSeq s = fromList (S.toList s)

  (* fromString : string -> char rope *)
    val fromString = fromList o String.explode

  (* tabulate : (int * (int -> 'a)) -> 'a rope *)
    fun tabulate (n, f) = fromList (List.tabulate (n, f))

  (* ***** ROPE DECONSTRUCTION ***** *)

  (* splitAtWithoutBalancing : 'a rope * int -> 'a rope * 'a rope *)
  (* pre: inBounds(r, i) *)
    fun splitAtWithoutBalancing (r, i) = 
     (case r
        of Leaf s => let
	     val (s1, s2) = S.cut (s, i+1)
	     in
	       (mkLeaf s1, mkLeaf s2)
	     end
	 | Cat (depth, len, r1, r2) =>
	     if i = length r1 - 1 then
               (r1, r2)
	     else if i < length r1 then let
               val (r11, r12) = splitAtWithoutBalancing(r1, i)
               in
                 (r11, appendWithoutBalancing(r12, r2))
               end
	     else let
               val (r21, r22) = splitAtWithoutBalancing(r2, i - length r1)
               in
                 (appendWithoutBalancing(r1, r21), r22)
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
  (* If a rope is a Cat, splits it at the root. *)
  (* If a rope is a Leaf, splits it into two leaves of roughly equal size. *)
    fun naturalSplit r =
     (case r
        of Leaf s => let
             val len' = S.length s div 2
	     val (s1, s2) = S.cut (s, len')
             in
               (mkLeaf s1, mkLeaf s2)
	     end
	 | Cat (_, _, r1, r2) => (r1, r2)
        (* end case *))

  (* subrope : 'a rope * int * int -> 'a rope *)
  (* Get the rope for indices [start,start+len-1]. *)
    fun subrope (r : 'a rope, start : int, len : int) : 'a rope = let
      fun get (Leaf s, st, len) =
            if st < S.length s andalso len + st - 1 < S.length s
	    then mkLeaf (S.subseq (s, st, len))
	    else raise Subscript
	| get (Cat (_, lenRope, rL, rR), st, len) =
            if len > lenRope
	    then raise Subscript
	    else let
              val lenL = length rL
              val nLeft = Int.min (len, lenL - st)
	      val nRight = len - nLeft
              val left = if nLeft = 0
			 then mkLeaf S.empty
			 else get (rL, st, nLeft)
	      val right = if nRight = 0
			  then mkLeaf S.empty
			  else get (rR, st + nLeft - lenL, nRight)
              in
                appendWithoutBalancing (left, right)
              end          
      in
        if start = 0 andalso len = length r
	then r
        else balanceIfNecessary (get (r, start, len))
      end

  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r] *)
    fun cut (r, n) =
	  if n = 0
	     then (mkLeaf S.empty, r)
	  else splitAt(r, n - 1)

  (* take : 'a rope * int -> 'a rope *)
    fun take (r, n) = #1(cut(r, n))

  (* drop : 'a rope * int -> 'a rope *)
    fun drop (r, n) = #2(cut(r, n))

  (* toList : 'a rope -> 'a list *)
    fun toList r = let
      fun build (Leaf s) = S.toList s
	| build (Cat (_, _, rL, rR)) = build rL @ build rR
      in
        build r
      end

  (* rev : 'a rope -> 'a rope *)
  (* pre: the input is balanced *)
  (* post: the output is balanced *)
    fun rev (Leaf s) = mkLeaf (S.rev s)
      | rev (Cat (d, len, r1, r2)) = Cat (d, len, rev r2, rev r1)

  (* map : ('a -> 'b) -> 'a rope -> 'b rope *)
  (* post : the output has the same shape as the input *)
    fun map f r = let
      fun m (Leaf s) = mkLeaf (S.map f s)
	| m (Cat (d, len, r1, r2)) = Cat (d, len, m r1, m r2)
      in
        m r
      end

  (* mapPartial : ('a -> 'b option) -> 'a rope -> 'b rope *)
    fun mapPartial f = let
      fun m (Leaf s) = let
            val s' = S.mapPartial f s
            in
	      mkLeaf s'
            end
	| m (Cat (_, _, rL, rR)) = appendWithoutBalancing (m rL, m rR)
      in
        balanceIfNecessary o m
      end

  (* filter : ('a -> bool) -> ('a rope -> 'a rope) *)
  (* post: the output is balanced *)
  (* Strategy: First, filter all the ropes and append without balancing. *)
  (*           Then balance the whole thing if needed. *)
    fun filter pred = let
      fun filt (Leaf s) = let
            val s' = S.filter pred s
            in
              mkLeaf s'
            end
	| filt (Cat (_, _, r1, r2)) = 
            appendWithoutBalancing (filt r1, filt r2)
      in
	balanceIfNecessary o filt
      end

  (* partition : ('a -> bool) -> 'a rope -> ('a rope * 'a rope) *)
    fun partition pred r = (filter pred r, filter (not o pred) r)

  (* foldl : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b *)
    fun foldl f z r = let
      fun fdl (Leaf s, acc) = S.foldl f acc s
	| fdl (Cat (_, _, rL, rR), acc) = fdl (rR, fdl (rL, acc))
      in
        fdl (r, z)
      end

  (* foldr : ('a * 'b -> 'b) -> 'b -> 'a rope -> 'b *)
    fun foldr f z (Leaf s) = S.foldr f z s
      | foldr f z (Cat (_, _, rL, rR)) = foldr f (foldr f z rR) rL

  (* app : ('a -> unit) -> 'a rope -> unit *)
    fun app f r = let
      fun go (Leaf s) = S.app f s
	| go (Cat (_, _, rL, rR)) = (go rL; go rR)
      in 
        go r 
      end

  (* find : ('a -> bool) -> 'a rope -> 'a option *)
    fun find pred r = let
      fun find (Leaf s) = S.find pred s
	| find (Cat (_, _, rL, rR)) = 
           (case find rL
	      of NONE => find rR
	       | something => something)
      in
	find r
      end

  (* exists : ('a -> bool) -> 'a rope -> bool *)
    fun exists pred r = let
      fun any (Leaf s) = S.exists pred s
	| any (Cat (_, _, rL, rR)) = any rL orelse any rR
      in
        any r
      end

  (* all : ('a -> bool) -> 'a rope -> bool *)
    fun all pred r = let
      fun all (Leaf s) = S.all pred s
	| all (Cat (_, _, rL, rR)) = all rL andalso all rR
      in
        all r
      end

  (* collate : ('a * 'a -> order) -> 'a rope * 'a rope -> order *)
    fun collate order (r0, r1) = List.collate order (toList r0, toList r1)

  end

(*
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
