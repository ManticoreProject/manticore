(* ropes-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ropes in SML.
 *)

functor RopesFn (

    structure S : SEQ

    val sizeL1CacheLine : int
    val wordSize        : int

  ) (* : ROPES *) = struct

    structure S = S
    type 'a seq = 'a S.seq

    datatype 'a rope
      = CAT of (int *                  (* depth *)
		int *                  (* length *)
		'a rope *              (* left subtree *)
		'a rope                (* right subtree *))
      | LEAF of (int *                 (* length *)
		 'a seq                (* sequence *))

    val maxLeafSize = sizeL1CacheLine

    val empty = LEAF(0, S.empty)

    fun isLeaf r = (
	  case r
	   of  LEAF _ => true
	     | CAT _ => false
          (* end case *)) 

  (* ceilingLg : int -> int *)
  (* ceiling of the log_2 of the input *)
    fun ceilingLg x = ceil (Math.ln (real x) / Math.ln 2.0)

  (* balancing condition for ropes *)
  (* the particular upper bound is claimed in Boehm et al. 95 *)
    fun isBalanced r = (
	  case r
	   of LEAF _ => true
	    | CAT(depth, len, _, _) => depth <= ceilingLg len + 2
          (* end case *))

    fun singleton x = LEAF (1, S.singleton x)

  (* toString : ('a -> string) -> 'a rope -> string *)
    fun toString show r = let
      fun copies thing n = List.tabulate (n, fn _ => thing)
      val rootString = "C<"
      val spaces = copies " "
      val indenter = String.concat (spaces (String.size rootString))
      (* indent : string list -> string list *)
      val indent = map (fn s => indenter ^ s) 
      (* build : 'a rope -> string list *)
      fun build (LEAF (_, xs)) = let 
            fun b ([], acc) = "]"::acc
	      | b ([x], acc) = b ([], (show x)::acc)
	      | b (x::xs, acc) = b (xs, ","::(show x)::acc)
            in
	      [(String.concat o rev) (b (S.toList xs, ["["]))]
            end
	| build (CAT (_, _, r1, r2)) = let 
            val ss1 = build r1
	    val ss2 = build r2
	    in
	      (indent ss1) @ (rootString :: (indent ss2))
	    end
    in
      String.concatWith "\n" (build r @ ["\n"])
    end

    fun isEmpty (LEAF (0, _)) = true
      | isEmpty (CAT (_, 0, _, _)) = true
      | isEmpty _ = false

    fun length r = (
	  case r
	   of LEAF (len, s) => len
	    | CAT(_, len, r1, r2) => len
          (* end case *))

    fun depth r = (
	  case r
	   of LEAF (_, _) => 0
	    | CAT(depth, _, _, _) => depth
          (* end case *))

    fun fib n = let
	  fun ff (0, u, p) = u
	    | ff (n, u, p) = ff (n-1, u+p, u)
        in
	  case Int.compare (n, 1)
	   of LESS => 0
	    | EQUAL => 1
	    | GREATER => ff (n, 0, 1)
        end  
  
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

  (* at each position the balancer contains
   *   - an inclusive lower bound on the rope length that may inhabit the 
   *      location (the inclusive lower bound is fib(n+1) where n is the index 
   *      of that spot)
   *   - an exclusive upper bound
   *   - some rope or none
   *)
    type 'a balancer = (int * int * 'a rope option) list

  (* takes a rope length, and returns a rope balancer *)
    fun mkInitialBalancer len = 
	  List.tabulate(balancerLen len, fn n => (fib(n + 2), fib(n + 3), NONE))

    fun rightmostLeaf r =
     (case r 
        of LEAF _ => r
	 | CAT (_, _, _, rR) => rightmostLeaf rR)

    fun leftmostLeaf r = 
     (case r
        of LEAF _ => r
	 | CAT (_, _, rL, _) => leftmostLeaf rL) 

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

  (* concatWithoutBalancing : 'a rope * 'a rope -> 'a rope *)
  (* concatenates two ropes (without balancing) *)
  (* handles some special cases: *)
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
	              val d   = maxLeafSize - S.length s1
		      val s1' = S.concat (s1, S.take (s2, d))
		      val s2' = S.drop (s2, d)
		      val lf1 = LEAF (maxLeafSize, s1')
		      val lf2 = LEAF (len2 - d, s2')
	              in
                        CAT (1, len1 + len2, lf1, lf2)
                      end
	       | (c as CAT (d, len1, r1, r2), leaf as LEAF (len2, s2)) => let
		    val rmost = rightmostLeaf r2
		    val n = length rmost + len2
		    in
		      if n <= maxLeafSize 
		      then CAT (d, len1 + len2, r1, attachRight (r2, s2))
		      else CAT (d+1, len1 + len2, c, leaf)
		    end
	       | (leaf as LEAF (len1, s1), c as CAT (d, len2, r1, r2)) => let
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

(* ... you can uncomment this for debugging ...
    fun concat (r1, r2) = let
      val r = concatWithoutBalancing (r1, r2)
      fun maxLeafSizeOf (r, acc) = 
       (case r
	  of LEAF (len, _) => if len>acc then len else acc
	   | CAT (_, _, r1, r2) => let
               val m1 = maxLeafSizeOf (r1, acc)
	       val m2 = maxLeafSizeOf (r2, acc)
	       in
		 if m1>m2 then m1 else m2
	       end)
      val m = maxLeafSizeOf (r, 0)
      in
	if m > maxLeafSize 
	then raise Fail (String.concat ["leaf got too big: ", Int.toString m])
	else r
      end
    val concatWithoutBalancing = concat
*)
               
  (* concatenate all ropes contained in the balancer into a single balanced rope *)
    fun concatBalancer balancer = let
	  fun f (b, acc) = (
	        case b
		 of (_, _, NONE) => acc
		  | (_, _, SOME r) => concatWithoutBalancing (r, acc)
  	        (* end case *))
          in
	    List.foldl f empty balancer
	  end

  (* insert a rope into a balancer. we maintain the invariant that the length of the rope
   * at position i is < #1(balancer[i]) (the includsive lower bound). see Boehm '95 for more
   * detail.
   *) 
    fun insert (r, balancer) = (
	  case balancer
	   of nil => (* this case should never be reached *)
	             raise Fail "BUG: empty balancer"
	    | [(lb, ub, NONE)] =>
                if length r >= lb andalso length r < ub then
                  [(lb, ub, SOME r)]
		else
                  raise Fail (String.concat ["BUG: trying to fit a rope of length ",
					     Int.toString (length r),
					     " into the interval [",
					     Int.toString lb,
					     ",",
					     Int.toString ub,
					     ")"])
	    | (lb, ub, NONE) :: t => 
	        if length r >= lb andalso length r < ub then 
                  (lb, ub, SOME r) :: t
		else 
		  (lb, ub, NONE) :: insert (r, t)
	    | b as ((lb, ub, SOME r') :: t) =>
                insert (concatWithoutBalancing (r', r), (lb, ub, NONE) :: t)
           (* end case *))

  (* takes a rope and returns the list of leaves in order *)
    fun leaves r = (
	  case r
	   of LEAF _ => r :: nil
	    | CAT (_, _, r1, r2) => leaves r1 @ leaves r2
          (* end case *))

  (* balance a rope. this operation is O(n*log n) in the number of leaves *)
    fun balance r = concatBalancer (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary r = if isBalanced r then r else balance r

  (* concatWithBalancing : 'a rope * 'a rope -> 'a rope *)
  (* concatenates two ropes (with balancing) *)
    fun concatWithBalancing (r1, r2) = balanceIfNecessary(concatWithoutBalancing(r1, r2))

    val concat = concatWithBalancing

  (* toSeq : 'a rope -> 'a seq *)
  (* return the fringe of the data at the leaves of a rope as a sequence *)
    fun toSeq r = 
     (case r
        of LEAF(_, s) => s
	 | CAT(_, _, r1, r2) => S.concat(toSeq r1, toSeq r2)
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

    fun inBounds (r, i) = i < length r andalso i >= 0

  (* pre: inBounds(r, i) *)
    fun subInBounds (r, i) = (
	  case r
	   of LEAF (_, s) => S.sub(s, i)
	    | CAT (depth, len, r1, r2) =>
	        if i < length r1
	           then subInBounds(r1, i)
		else subInBounds(r2, i - length r1)
          (* end case *))

  (* subscript; returns r[i] *)
    fun sub (r, i) = 
	  if inBounds(r, i)
	     then subInBounds(r, i)
	  else raise Fail "subscript out of bounds"

  (* pre: inBounds(r, i) *)
    fun splitAtWithoutBalancing (r, i) = (
	  case r
	   of LEAF (len, s) => let
	        val (s1, s2) = S.splitAt(s, i)
	        in
		  (LEAF (i + 1, s1), LEAF (len - i - 1, s2))
	        end
	    | CAT (depth, len, r1, r2) =>
	        if i < length r1
	           then let
		      val (r11, r12) = splitAtWithoutBalancing(r, i)
		      in
			(r11, concatWithoutBalancing(r12, r2))
		      end
	        else let
		    val (r21, r22) = splitAtWithoutBalancing(r, i)
		    in
		      (concatWithoutBalancing(r1, r21), r22)
		    end
          (* end case *))

  (* pre: inBounds(r, i) *)
    fun splitAtWithBalancing (r, i) = let
	  val (r1, r2) = splitAtWithoutBalancing(r, i)
          in
	    (balanceIfNecessary r1, balanceIfNecessary r2)
	  end

  (* split a rope in two at index i. (r[0, ..., i], r[i+1, ..., n]) *)
    fun splitAt (r, i) =
	  if inBounds(r, i)
	     then splitAtWithBalancing(r, i)
	  else raise Fail "subscript out of bounds for splitAt"

  (* the following functions are for exploring the possibility of *)
  (*   balancing ropes in parallel: *)

  (* merge two balancers *)
    fun merge (b1, b2) =
	  insert(concatBalancer b2, insert(concatBalancer b1, mkInitialBalancer (List.length b1)))

  (* merge a rope of ropes into a balancer *)
    fun mergeRope r = (
	  case r
	   of LEAF(_, r') => mkInitialBalancer 32
	    | CAT(_, _, r1, r2) => merge(mergeRope r1, mergeRope r2)
	  (* end case *))

  (* concat a rope of ropes into a single rope *)
    fun concatN r = concatBalancer(mergeRope r)

  end
