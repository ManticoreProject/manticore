(* float-rope.pml  
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Monomorphic float ropes.
 *)

structure FloatRope = struct

    structure S = FloatArraySeq

    datatype option = datatype Option.option

    type seq = S.seq

  (* ***** UTILITIES ***** *)

  (* failwith : string -> 'a *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

  (* ***** ROPES ***** *)

  (* The rope datatype and some basic operations. *)

    datatype float_rope
      = CAT of (int *     (* depth *)
		int *     (* length *)
		float_rope * (* left subtree *)
		float_rope   (* right subtree *))
      | LEAF of (int *    (* length *)
		 seq      (* sequence *))

  (* maxLeafSize : int *)
    val maxLeafSize = MaxLeafSize.sz

  (* empty : float_rope *)
    val empty = LEAF (0, S.empty)

  (* toString : float_rope -> string *)
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
		  | (x::nil, acc) => b (nil, Float.toString x :: acc)
		  | (x::xs, acc) => b (xs, "," :: Float.toString x ::acc)
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

  (* isLeaf : float_rope -> bool *)
    fun isLeaf r = 
     (case r
        of LEAF _ => true
	 | CAT _ => false
        (* end case *)) 

  (* isBalanced : 'float_rope -> bool *)
  (* balancing condition for ropes *)
  (* The max depth here is given in Boehm et al. 95. *)
    fun isBalanced r = 
     (case r
        of LEAF _ => true
	 | CAT (depth, len, _, _) => (depth <= Int.ceilingLg len + 2)
        (* end case *))

  (* singleton : float -> float_rope *)
    fun singleton x = LEAF (1, S.singleton x)

  (* length : float_rope -> int *)
    fun length r = 
     (case r
        of LEAF (len, s) => len
	 | CAT(_, len, r1, r2) => len
        (* end case *))

  (* isEmpty : float_rope -> bool *)
    fun isEmpty r = (length r = 0)

  (* depth : float_rope -> int *)
  (* The depth of a leaf is 0. *)
    fun depth r = 
     (case r
        of LEAF _ => 0
	 | CAT(depth, _, _, _) => depth
        (* end case *))

  (* inBounds : float_rope * int -> bool *)
  (* Is the given int a valid index of the rope at hand? *)
    fun inBounds (r, i) = i >= 0 andalso i < length r

  (* subInBounds : float_rope * int -> float *)
  (* pre: inBounds (r, i) *)
    fun subInBounds (r, i) = 
     (case r
        of LEAF (_, s) => S.sub (s, i)
	 | CAT (depth, len, r1, r2) =>
	     if i < length r1 then 
               subInBounds(r1, i)
	     else 
               subInBounds(r2, i - length r1)
        (* end case *))

  (* sub : float_rope * int -> float *)
  (* subscript; returns r[i] *)
    fun sub (r, i) = 
      if inBounds (r, i) 
      then subInBounds(r, i)
      else failwith "subscript out of bounds"

  (* ***** BALANCING ***** *)

  (* We follow the rope balancing algorithm given in Boehm et al. 1995 *)

  (* That algorithm requires a data structure we call a "balancer". *)
    type balancer = (int * int * float_rope option) list

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

  (* leftmostLeaf : float_rope -> float_rope *)
    fun leftmostLeaf r = 
     (case r
        of LEAF _ => r
	 | CAT (_, _, rL, _) => leftmostLeaf rL) 

  (* rightmostLeaf : float_rope -> float_rope *)
    fun rightmostLeaf r =
     (case r 
        of LEAF _ => r
	 | CAT (_, _, _, rR) => rightmostLeaf rR)

  (* attachLeft : seq * float_rope -> float+rope *)
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

  (* attachRight : float_rope * seq -> float_rope *) 
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

  (* concatWithoutBalancing : float_rope * float_rope -> float_rope *)
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
 
  (* balToRope : balancer -> float_rope *)              
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

  (* insert : float_rope * balancer -> balancer *)
  (* Insert a rope into a balancer. *)
  (* invariant: the length of the rope at position i is in its interval, that is, *)
  (*   greater than or equal to the lower bound, and less that the upper bound. *)
  (* See Boehm et al. '95 for details. *)
    fun insert (r, balancer) = 
     (case balancer
        of nil => (* this case should never be reached *)
	          (failwith "BUG: empty balancer")
	 | (lb, ub, NONE) :: nil =>
             if length r >= lb andalso length r < ub then
               (lb, ub, SOME r)::nil
	     else 
               (failwith "BUG: typing to fit a rope of incompatible size")
	 | (lb, ub, NONE) :: t => 
	     if length r >= lb andalso length r < ub then 
               (lb, ub, SOME r) :: t
	     else 
               (lb, ub, NONE) :: insert (r, t)
	 | (lb, ub, SOME r') :: t =>
             insert (concatWithoutBalancing (r', r), (lb, ub, NONE) :: t)
        (* end case *))

  (* leaves : float_rope -> float_rope list *)
  (* takes a rope and returns the list of leaves in left-to-right order *)
    fun leaves r = 
     (case r
        of LEAF _ => r :: nil
	 | CAT (_, _, r1, r2) => leaves r1 @ leaves r2
        (* end case *))

  (* balance : float_rope -> float_rope *)
  (* Balance a rope to within 2 of ideal depth. *)
  (* This operation is O(n*log n) in the number of leaves *)
    fun balance r = balToRope (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balanceIfNecessary : float_rope -> float_rope *)
  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary r = if isBalanced r then r else balance r

  (* ***** ROPE CONSTRUCTION ***** *)

  (* concatWithBalancing : float_rope * float_rope -> float_rope *)
  (* concatenates two ropes (with balancing) *)
    fun concatWithBalancing (r1, r2) = balanceIfNecessary (concatWithoutBalancing(r1, r2))

  (* concat : float_rope -> float_rope *)
    val concat = concatWithBalancing

  (* ignore : 'a -> unit *)
    fun ignore x = ()

  (* toSeq : float_rope -> seq *)
  (* return the fringe of the data at the leaves of a rope as a sequence *)
    fun toSeq r = let
      val len = length r
      val s = S.tabulate (len, fn _ => 0.0)
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

  (* catPairs :  float_rope list -> float_rope list *)
  (* Concatenate every pair of ropes in a list. *)
  (* ex: catPairs [r0,r1,r2,r3] => [Cat(r0,r1),Cat(r2,r3)] *)
    fun catPairs rs = 
     (case rs
        of nil => nil
	 | r::nil => rs
	 | r0::r1::rs => (concatWithoutBalancing (r0, r1)) :: catPairs rs
       (* end case *))

  (* leafFromList : float list -> float_rope *)
    fun leafFromList (xs : 'a list) = let
      val n = List.length xs
      in
        if n <= maxLeafSize then
          LEAF (n, S.fromList xs)
        else
          failwith "too big"
      end

  (* fromList : float list -> float_rope *)
  (* Given a list, construct a balanced rope. *)
  (* The leaves will be packed to the left.  *)
    fun fromList xs = let
      val ldata = chop (xs, maxLeafSize)
      val leaves = List.map leafFromList ldata
      fun build ls = 
       (case ls
          of nil => empty
           | l::nil => l
           | _ => build (catPairs ls)
         (* end case *))
      in
        build leaves      
      end

  (* fromSeq : seq -> float_rope *)
  (* FIXME bad implementation *)
    fun fromSeq s = fromList (S.toList s)

  (* tabFromToP : int * int * (int -> float) -> float_rope *)
  (* pre: hi >= lo *)
  (* lo inclusive, hi exclusive *)
    fun tabFromToP (lo, hi, f) = 
     (if lo > hi then
       (failwith "downward tabulate")
      else if (hi - lo) <= maxLeafSize then let
        fun f' n = f (n + lo)
        in
          LEAF (hi-lo, S.tabulate (hi-lo, f'))
        end
      else let
        val m = (hi + lo) div 2
        in
	  concatWithoutBalancing (| tabFromToP (lo, m, f),
				    tabFromToP (m, hi, f) |)
        end)

  (* tabP : int * (int -> float) -> float_rope *)
    fun tabP (n, f) = 
     (if n <= 0 then
        empty
      else
        tabFromToP (0, n, f) (* n.b.: tabFromToP is exclusive of its upper bound *))

(* ***** ROPE DECONSTRUCTION ***** *)

  (* splitAtWithoutBalancing : float_rope * int -> float_rope * float_rope *)
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

  (* splitAtWithBalancing : float_rope * int -> float_rope * float_rope *)
  (* pre: inBounds (r, i) *)
    fun splitAtWithBalancing (r, i) = let
      val (r1, r2) = splitAtWithoutBalancing (r, i)
      in
        (balanceIfNecessary r1, balanceIfNecessary r2)
      end

  (* splitAt : float_rope * int -> float_rope * float_rope *)
  (* split a rope in two at index i. (r[0, ..., i], r[i+1, ..., |r|-1]) *)
    fun splitAt (r, i) =
      if inBounds(r, i)
      then splitAtWithBalancing(r, i)
      else failwith "subscript out of bounds for splitAt"

  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r - 1] *)
    fun cut (r, n) =
      if n = 0
      then (empty, r)
      else splitAt(r, n - 1)

  (* naturalSplit : float_rope -> float_rope * float_rope *)
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

  (* partialSeq : float_rope * int * int -> seq *)
  (* return the sequence of elements from low incl to high excl *)
  (* zero-based *)
  (* failure when lower bound is less than 0  *)
  (* failure when upper bound is off the rope (i.e., more than len rope + 1) *)
    fun partialSeq (r, lo, hi) =
     (case r
        of LEAF (len, s) => 
            (if lo >= len orelse hi > len then
               failwith "err"
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

  (* revP : float_rope -> float_rope *)
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

  (* mapP : (float -> float) * float_rope -> float_rope *)
  (* post : the output has the same shape as the input *)
    fun mapP (f, rope) = let
      fun m r =
       (case r
          of LEAF (len, s) => LEAF (len, S.map (f, s))
	   | CAT (dpt, len, r1, r2) => CAT (| dpt, len, m r1, m r2 |)
          (* end case *))
      in
        m rope
      end          

  (* mapPolyP : (float -> float) * float_rope -> float_rope *)
  (* post : the output has the same shape as the input *)
    fun mapPolyP (f, rope) = let
      fun m r =
       (case r
          of LEAF (len, s) => Rope.LEAF (len, S.mapPoly (f, s))
	   | CAT (dpt, len, r1, r2) => Rope.CAT (| dpt, len, m r1, m r2 |)
         (* end case *))
      in
        m rope
      end          

  (* sumP : float_rope -> float *)
    fun sumP rope = let
      fun add (x:float, y:float) = x+y
      fun s r =
       (case r
          of LEAF (_, s) => S.sum s
           | CAT (_, _, rL, rR) => add (| s rL, s rR |)
         (* esac *))
      in
        s rope
      end

  (* reduceP : (float * float -> float) * float * float_rope -> float *)
  (* Reduce with an associative operator. *)
  (* e.g., sumP r == reduceP (+, 0, r) *)
    fun reduceP (assocOp, unit, rope) = let
      fun red r =
       (case r
	  of LEAF (_, s) => S.reduce (assocOp, unit, s)
	   | CAT (_, _, r1, r2) => assocOp (| red r1, red r2 |)
         (* end case *))
      in
        red rope
      end

  (* filterP : (float -> bool) * float_rope -> float_rope *)
  (* post: the output is balanced *)
  (* Strategy: First, filter all the leaves without balancing. *)
  (*           Then balance the whole thing if needed. *)
    fun filterP (pred, rope) = let
      fun f r =
       (case r
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

  end
