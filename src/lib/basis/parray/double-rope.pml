(* double-rope.pml  
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Monomorphic ropes of double.
 *)

structure DoubleRope = struct

    structure S   = DoubleArraySeq
    structure SPr = DoubleArraySeqPair

    type seq = S.seq

  (* ***** UTILITIES ***** *)

  (* fail : string -> string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    val fail = Fail.fail "DoubleRope"

  (* ***** ROPES ***** *)

  (* The rope datatype and some basic operations. *)

    datatype double_rope
      = CAT of (int *         (* depth *)
		int *         (* length *)
		double_rope * (* left subtree *)
		double_rope   (* right subtree *))
      | LEAF of (int *        (* length *)
		 seq          (* sequence *))

  (* maxLeafSize : int *)
    val maxLeafSize = MaxLeafSize.sz

  (* empty : double_rope *)
    val empty = LEAF (0, S.empty)

  (* toString : double_rope -> string *)
    fun toString r = let
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
		  | (x::nil, acc) => b (nil, Double.toString x :: acc)
		  | (x::xs, acc) => b (xs, "," :: Double.toString x ::acc)
	         (* end case *))
              in
		(String.concat (List.rev (b (S.toList xs, ("["::nil))))) :: nil
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

  (* isLeaf : double_rope -> bool *)
    fun isLeaf r = 
     (case r
        of LEAF _ => true
	 | CAT _ => false
        (* end case *)) 

  (* isBalanced : double_rope -> bool *)
  (* balancing condition for ropes *)
  (* The max depth here is given in Boehm et al. 95. *)
    fun isBalanced r = 
     (case r
        of LEAF _ => true
	 | CAT (depth, len, _, _) => (depth <= Int.ceilingLg len + 2)
        (* end case *))

  (* singleton : double -> double_rope *)
    fun singleton x = LEAF (1, S.singleton x)

  (* length : double_rope -> int *)
    fun length r = (case r
      of LEAF (len, s) => len
       | CAT(_, len, r1, r2) => len
      (* end case *))

  (* isEmpty : double_rope -> bool *)
    fun isEmpty r = (length r = 0)

  (* depth : double_rope -> int *)
  (* The depth of a leaf is 0. *)
    fun depth r = 
     (case r
        of LEAF _ => 0
	 | CAT(depth, _, _, _) => depth
        (* end case *))

  (* inBounds : double_rope * int -> bool *)
  (* Is the given int a valid index of the rope at hand? *)
    fun inBounds (r, i) = i >= 0 andalso i < length r

  (* subInBounds : double_rope * int -> double *)
  (* pre: inBounds (r, i) *)
    fun subInBounds (r, i) = (case r
      of LEAF (_, s) => S.sub (s, i)
       | CAT (depth, len, r1, r2) =>
	   if i < length r1 then 
             subInBounds(r1, i)
	   else 
             subInBounds(r2, i - length r1)
      (* end case *))

  (* sub : double_rope * int -> double *)
  (* subscript; returns r[i] *)
    fun sub (r, i) = 
      if inBounds (r, i) 
      then subInBounds(r, i)
      else fail "sub" "subscript out of bounds"

  (* ***** BALANCING ***** *)

  (* We follow the rope balancing algorithm given in Boehm et al. 1995 *)

  (* That algorithm requires a data structure we call a "balancer". *)
    type balancer = (int * int * double_rope option) list

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
	        if Int.fib n > len
		   then n
		else lp (n + 1)
          in
	    lp 0 - 2
	  end

  (* mkInitialBalancer : int -> balancer *)
  (* takes a rope length, and returns a rope balancer *)
    fun mkInitialBalancer len = let
      val blen = balancerLen len
      fun initEntry n = (Int.fib (n+2), Int.fib (n+3), NONE)
      in
        List.tabulate (blen, initEntry)
      end

  (* leftmostLeaf : double_rope -> double_rope *)
    fun leftmostLeaf r = 
     (case r
        of LEAF _ => r
	 | CAT (_, _, rL, _) => leftmostLeaf rL) 

  (* rightmostLeaf : double_rope -> double_rope *)
    fun rightmostLeaf r =
     (case r 
        of LEAF _ => r
	 | CAT (_, _, _, rR) => rightmostLeaf rR)

  (* attachLeft : seq * double_rope -> double_rope *)
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

  (* attachRight : double_rope * seq -> double_rope *) 
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

  (* concatWithoutBalancing : double_rope * double_rope -> double_rope *)
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
     (if isEmpty r1 then r2 
      else if isEmpty r2 then r1
      else (case (r1, r2)
        of (LEAF (len1, s1), LEAF (len2, s2)) =>
	     if (len1 + len2) <= maxLeafSize then
               LEAF (len1 + len2, S.concat (s1, s2))
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

  (* balToRope : balancer -> double_rope *)              
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

  (* insert : double_rope * balancer -> balancer *)
  (* Insert a rope into a balancer. *)
  (* invariant: the length of the rope at position i is in its interval, that is, *)
  (*   greater than or equal to the lower bound, and less that the upper bound. *)
  (* See Boehm et al. '95 for details. *)
    fun insert (r, balancer) = 
     (case balancer
        of nil => (* this case should never be reached *)
	     fail "insert" "empty balancer"
	 | (lb, ub, NONE) :: nil =>
             if length r >= lb andalso length r < ub then
               (lb, ub, SOME r)::nil
	     else 
               fail "insert" "BUG: typing to fit a rope of incompatible size"
	 | (lb, ub, NONE) :: t => 
	     if length r >= lb andalso length r < ub then 
               (lb, ub, SOME r) :: t
	     else 
               (lb, ub, NONE) :: insert (r, t)
	 | (lb, ub, SOME r') :: t =>
             insert (concatWithoutBalancing (r', r), (lb, ub, NONE) :: t)
        (* end case *))

  (* leaves : double_rope -> double_rope list *)
  (* takes a rope and returns the list of leaves in left-to-right order *)
    fun leaves r = 
     (case r
        of LEAF _ => r :: nil
	 | CAT (_, _, r1, r2) => leaves r1 @ leaves r2
        (* end case *))

  (* balance : double_rope -> double_rope *)
  (* Balance a rope to within 2 of ideal depth. *)
  (* This operation is O(n*log n) in the number of leaves *)
    fun balance r = balToRope (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balanceIfNecessary : double_rope -> double_rope *)
  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary r = if isBalanced r then r else balance r

  (* ***** ROPE CONSTRUCTION ***** *)

  (* mkLeaf : seq -> double_rope *)
    fun mkLeaf s = let
      val n = S.length s
      in
        if (n > maxLeafSize) then
          fail "mkLeaf" "too many elements"
	else
          LEAF (n, s)
      end

  (* concatWithBalancing : double_rope * double_rope -> double_rope *)
  (* concatenates two ropes (with balancing) *)
    fun concatWithBalancing (r1, r2) = balanceIfNecessary (concatWithoutBalancing(r1, r2))

  (* concat : double_rope -> double_rope *)
    val concat = concatWithBalancing

  (* toSeq : float_rope -> seq *)
  (* return the fringe of the data at the leaves of a rope as a sequence *)
    fun toSeq r = let
      val len = length r
      val s = S.tabulate (len, fn _ => 0)
      fun loop (i, r) =
       (case r
          of LEAF (n, data) =>
               if i >= len then ()
	       else let
                 fun lp (d_i, s_i) = 
                   if s_i >= (i+n) then ()
		   else let
                     val _ = S.update (s, s_i, S.sub (data, d_i))
		     in
                       lp (d_i+1, s_i+1)
		     end
                in
                  lp (0, i)
	        end
	   | CAT (_, n, rL, rR) => (loop (i, rL); 
				    loop (i + length rL, rR))
         (* esac *))
    val _ = loop (0, r)
    in
      s
    end

  (* split :  'a list * int -> 'a list * 'a list *)
  (* Split the list into two pieces. *)
  (* Don't complain if there aren't enough elements. *)
  (* ex: split ([1,2,3], 0) => ([],[1,2,3]) *)
  (* ex: split ([1,2,3], 1) => ([1],[2,3])  *)
  (* ex: split ([1,2,3], 2) => ([1,2],[3])  *)
  (* ex: split ([1,2,3], 4) => ([1,2,3],[]) *)
    fun split (xs, n) = let
      fun loop (n, taken, xs) =
       (case xs
          of nil => (List.rev taken, nil)
         | h::t => if n = 0 then
                        (List.rev taken, xs)
            else
                        loop (n-1, h::taken, t)
          (* end case *))
      in
        if n <= 0 then
          (nil, xs)
        else
          loop (n, nil, xs)
      end
         
  (* chop : 'a list * int -> 'a list list *)
  (* Chop the list into pieces of the appropriate size. *)
  (* Doesn't complain if the chopping is uneven (see 3rd ex.). *)
  (* ex: chop ([1,2,3,4], 1) => [[1],[2],[3],[4]] *)
  (* ex: chop ([1,2,3,4], 2) => [[1,2],[3,4]] *)
  (* ex: chop ([1,2,3,4], 3) => [[1,2,3],[4]] *)
    fun chop (xs, sz) = let
      fun lp arg = 
       (case arg
	  of (nil, acc) => List.rev acc
	   | (ns, acc) => let
               val (t, d) = split (ns, sz)
               in
                 lp (d, t::acc)
               end	 
         (* end case *))
      in
        lp (xs, nil)
      end

  (* catPairs :  double_rope list -> double_rope list *)
  (* Concatenate every pair of ropes in a list. *)
  (* ex: catPairs [r0,r1,r2,r3] => [Cat(r0,r1),Cat(r2,r3)] *)
    fun catPairs rs = 
     (case rs
        of nil => nil
	 | r::nil => rs
	 | r0::r1::rs => (concatWithoutBalancing (r0, r1)) :: catPairs rs
       (* end case *))

  (* leafFromList : double list -> double_rope *)
    fun leafFromList (xs : double list) = let
      val n = List.length xs
      in
        if n <= maxLeafSize then
          LEAF (n, S.fromList xs)
        else
          fail "leafFromList" "too many elements"
      end

  (* fromList : double list -> double_rope *)
  (* Given a list, construct a balanced rope. *)
  (* The leaves will be packed to the left.  *)
    fun fromList (xs : double list) = let
      val ldata = chop (xs, maxLeafSize)
      val leaves = List.map leafFromList ldata
      fun build ls = (case ls
        of nil => empty
         | l::nil => l
         | _ => build (catPairs ls)
        (* end case *))
      in
        build leaves      
      end

  (* fromSeq : seq -> double_rope *)
    fun fromSeq s = fromList (S.toList s)

  (* tabFromToP : int * int * (int -> double) -> double_rope *)
  (* pre: hi >= lo *)
  (* lo inclusive, hi inclusive *)
    fun tabFromToP (lo, hi, f : int -> double) = 
      if lo > hi then
        empty
      else let
        val nElts = hi-lo+1
        in
          if nElts <= maxLeafSize then let
            fun f' n = f (n + lo)
            in
              LEAF (nElts, S.tabulate_double (nElts, f'))
            end
	  else let
            val m = (hi + lo) div 2
            in
	      concatWithoutBalancing (| tabFromToP (lo, m, f),
	 			        tabFromToP (m+1, hi, f) |)
            end
	end

  (* tabP : int * (int -> double) -> double_rope *)
    fun tabP (n, f : int -> double) = tabFromToP (0, n-1, f)

  (* tabFromToStepP : int * int * int * (int -> double) -> double_rope *)
  (* lo inclusive, hi inclusive *)
    fun tabFromToStepP (from, to_, step, f : int -> double) = let
      fun f' i = f (from + (step * i))
      in (case Int.compare (step, 0)
        of EQUAL => fail "tabFromToStepP" "0 step"
	 | LESS (* negative step *) =>
             if (to_ > from) then
               empty
       	     else
               tabFromToP (0, (from-to_) div (~step), f')
	 | GREATER (* positive step *) =>
       	     if (from > to_) then
       	       empty
       	     else
               tabFromToP (0, (to_-from) div step, f')
	(* end case *))
      end

  (* leafFromSeq : double ArraySeq.seq -> seq *)
    fun leafFromSeq s = let
      val a = ArraySeq.toArray s
      val n = Array.length a
(* FIXME rewrite to ropeFromSeq *)
      val a' = DoubleArray.tabulate (n, fn i => Array.sub (a, i))
      in
        if (n > maxLeafSize) 
	then fail "leafFromSeq" "too many elements"
        else LEAF (n, a')
      end

(* ***** ROPE DECONSTRUCTION ***** *)

  (* splitAtWithoutBalancing : double_rope * int -> double_rope * double_rope *)
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

  (* splitAtWithBalancing : double_rope * int -> double_rope * double_rope *)
  (* pre: inBounds (r, i) *)
    fun splitAtWithBalancing (r, i) = let
      val (r1, r2) = splitAtWithoutBalancing (r, i)
      in
        (balanceIfNecessary r1, balanceIfNecessary r2)
      end

  (* splitAt : double_rope * int -> double_rope * double_rope *)
  (* split a rope in two at index i. (r[0, ..., i], r[i+1, ..., |r|-1]) *)
    fun splitAt (r, i) =
      if inBounds(r, i)
      then splitAtWithBalancing(r, i)
      else fail "splitAt" "subscript out of bounds"

  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r - 1] *)
    fun cut (r, n) =
      if n = 0
      then (empty, r)
      else splitAt(r, n - 1)

  (* naturalSplit : double_rope -> double_rope * double_rope *)
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

  (* partialSeq : double_rope * int * int -> seq *)
  (* return the sequence of elements from low incl to high excl *)
  (* zero-based *)
  (* failure when lower bound is less than 0  *)
  (* failure when upper bound is off the rope (i.e., more than len rope + 1) *)
    fun partialSeq (r, lo, hi) =
     (case r
        of LEAF (len, s) => 
            (if lo >= len orelse hi > len then
               fail "partialSeq" "subscript out of bounds"
	     else
	       S.take (S.drop (s, lo), hi-lo))
	 | CAT (_, len, rL, rR) => let
             val lenL = length rL
	     val lenR = length rR
	     in
	       if hi <= lenL then (* everything's on the left *)
		   partialSeq (rL, lo, hi)
	       else if lo >= lenL then (* everything's on the right *)
		   partialSeq (rR, lo-lenL, hi-lenL)
	       else let
                 val sL = partialSeq (rL, lo, lenL)
		 val sR = partialSeq (rR, 0, hi-lenL)
                 in
		   S.concat (sL, sR)
		 end
	     end
        (* end case *))

  (* ***** BASIC PARALLEL OPERATIONS ***** *)

  (* FIXME TODO No account is yet taken of the "leftmost exception" semantic property. *)

  (* revP : double_rope -> double_rope *)
  (* pre  : the input is balanced *)
  (* post : the output is balanced *)
    fun revP r = 
     (case r
        of LEAF (len, s) => LEAF (len, S.rev s)
	 | CAT (dpt, len, r1, r2) => let
	     val (r1, r2) = (| revP r1, revP r2 |)
	     in
	       CAT (dpt, len, r2, r1)
	     end
        (* end case *))

  (* mapP : (double -> double) * double_rope -> double_rope *)
  (* post : the output has the same shape as the input *)
    fun mapP (f : double -> double, rope) = let
      fun m r =
       (case r
          of LEAF (len, s) => LEAF (len, S.map (f, s))
	   | CAT (dpt, len, r1, r2) => CAT (| dpt, len, m r1, m r2 |)
          (* end case *))
      in
        m rope
      end          

  (* sumP : double_rope -> double *)
    fun sumP rope = let
      fun add (x:double, y:double) = x+y
      fun s r = (case r
        of LEAF (_, s) => S.sum s
         | CAT (_, _, rL, rR) => add (| s rL, s rR |)
        (* end case *))
      in
        s rope
      end

  (* reduceP : (double * double -> double) * double * double_rope -> double *)
  (* Reduce with an associative operator. *)
  (* e.g., sumP r == reduceP (+, 0.0, r) *)
    fun reduceP (assocOp, unit, rope) = let
      fun red r = (case r
        of LEAF (_, s) => S.reduce (assocOp, unit, s)
	 | CAT (_, _, r1, r2) => assocOp (| red r1, red r2 |)
        (* end case *))
      in
        red rope
      end

  (* filterP : (double -> bool) * double_rope -> double_rope *)
  (* post: the output is balanced *)
  (* Strategy: First, filter all the leaves without balancing. *)
  (*           Then balance the whole thing if needed. *)
    fun filterP (pred, rope) = let
      fun f r = (case r
        of LEAF (len, s) => let
             val s' = S.filter (pred, s)
             in
               LEAF (S.length s', s')
	     end
	 | CAT (_, _, r1, r2) => 
	     concatWithoutBalancing (| f r1, f r2 |)
        (* end case *))
      in
        balanceIfNecessary (f rope)
      end

  (* app : (double -> unit) * double_rope -> unit *)
    fun app (f : double -> unit, rope) = let
      fun appF (LEAF (_, s)) = S.app (f, s)
	| appF (CAT (_, _, r1, r2)) = (appF r1; appF r2)
      in
        appF rope
      end

  (* nEltsInRange : int * int * int -> int *)
    fun nEltsInRange (from, to_, step) = (* "to" is syntax in pml *)
	  if step = 0 then fail "nEltsInRange" "cannot have step 0 in a range"
	  else if from = to_ then 1
	  else if (from > to_ andalso step > 0) then 0
	  else if (from < to_ andalso step < 0) then 0
	  else (Int.abs (from - to_) div Int.abs step) + 1

(* 
  (* double ranges not supported yet *)
  (* rangeP : int * int * int -> int rope *)
  (* note: both from and to are inclusive bounds *)
    fun rangeP (from, to_, step) = (* "to" is syntax in pml *)
     (if from = to_ then singleton from
      else let
        val sz = nEltsInRange (from, to_, step)
        fun gen n = step * n + from
        in
          tabP (sz, gen)
        end)
*)

  end