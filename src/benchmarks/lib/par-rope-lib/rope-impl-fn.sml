(* rope-impl-fn.sml
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

functor RopeImplFn (

    structure S : SEQ

    val maxLeafSize : int

    structure RT : RUNTIME

  ) = struct

    structure S = S
    type 'a seq = 'a S.seq

    structure RTy = RopeTyFn (
                       structure S = S)
    datatype rope = datatype RTy.rope

    datatype either = datatype Either.either
    datatype progress = datatype ProgressTy.progress

    val _ = if maxLeafSize < 1 then raise Fail "bogus max leaf size" else ()

  (* ***** UTILITIES ***** *)

 
(*slower way of computing log2*)
(*
    fun log base x = Math.ln x / Math.ln base
    val ceilLog2 : int -> int = ceil o log 2.0 o real 
*)

    local
	structure W = Word32
	fun toWord w = Word.fromLarge(W.toLarge w)
    in

     (* compute the number of leading zeros in a word;
      * this code was translated from the C code in
      * Figure 5.10 (p.80) of
      *
      *    Hackers Delight
      *    by Henry S. Warren, Jr.
      *    Pearson Education Inc, 2003.
      *)
       fun nlz x = let
	     val y = W.~ (W.>> (x, 0w16))
	     val m = W.andb(W.>>(y, 0w16), 0w16)
	     val n = 0w16 - m
	     val x = W.>>(x, toWord m)
	     val y = x - 0wx100
	     val m = W.andb(W.>>(y, 0w16), 0w8)
	     val n = n + m
	     val x = W.<<(x, toWord m)
	     val y = x - 0wx1000
	     val m = W.andb(W.>>(y, 0w16), 0w4)
	     val n = n + m
	     val x = W.<<(x, toWord m)
	     val y = x - 0wx4000
	     val m = W.andb(W.>>(y, 0w16), 0w2)
	     val n = n + m
	     val x = W.<<(x, toWord m)
	     val y = W.>>(x, 0w14)
	     val m = W.andb(y, W.notb(W.>>(y, 0w1)))
	     in
	       W.toIntX (n + 0w2 - m)
	     end
       fun ceilLog2 x = (31 - nlz(W.fromInt(x-1))) + 1
    end (* local *)

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
  
  (* Split the list into two pieces. *)
  (* This function is basically take and drop at the same time. *)
  (* It doesn't raise an exn if there aren't enough elements. *)
  (* ex: split ([1,2,3], 0) => ([],[1,2,3]) *)
  (* ex: split ([1,2,3], 1) => ([1],[2,3])  *)
  (* ex: split ([1,2,3], 2) => ([1,2],[3])  *)
  (* ex: split ([1,2,3], 4) => ([1,2,3],[]) *)
    fun split (xs : 'a list, n : int) : ('a list * 'a list) = let
      fun loop (n, taken, xs) =
	  (case xs
	    of nil => (List.rev taken, nil)
	     | h::t =>
               if n = 0 then (List.rev taken, xs)
               else loop (n-1, h::taken, t)
	  (* end case *))
      in
        if n <= 0 
	then (nil, xs)
	else loop (n, nil, xs)
      end
         
  (* ***** ROPES ***** *)

  (* maxLeafSize : int *)
    val maxLeafSize = maxLeafSize

  (* this constructor maintains the invariant that the leaf size must be *)
  (* <= to the constant maxLeafSize *)
    fun mkLeaf (seq : 'a seq) : 'a rope = 
	  if S.length seq > maxLeafSize 
	    then raise Fail "RopeFn.mkLeaf: bogus leaf size"
	  else Leaf seq

    fun empty () : 'a rope = Leaf (S.empty ())
    fun depth rp = (case rp
		     of Leaf _ => 0
		      | Cat (d, _, _, _) => d
		   (* end case *))

  (* returns a string displaying the internal structure of the given rope *)
    fun toString (show : 'a -> string) (r : 'a rope) : string = let
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

    fun isLeaf rp = (case rp
		      of Leaf _ => true
		       | _ => false
		    (* end case *))

  (* balancing condition for ropes *)
  (* The max depth here is given in \cite{bap:ropes}. *)
    fun isBalanced rp = 
	(case rp
	  of Leaf _ => true
	   | Cat (depth, len, _, _) => (depth <= (ceilLog2 len) + 2)
	(* end case *))

  (* creates a rope containing just x *)
    fun singleton x = Leaf (S.singleton x)

    fun isEmpty rp = (case rp
		       of Leaf seq => S.length seq = 0
			| Cat (_, len, _, _) => len = 0
		     (* end case *))

    fun length rp = (case rp
		      of Leaf seq => S.length seq
		       | Cat (_, len, _, _) => len
		    (* end case *))

    fun depth rp = (case rp
		     of Leaf _ => 0
		      | Cat (d, _, _, _) => d
		   (* end case *))

  (* Is the given int a valid index of the given rope? *)
    fun inBounds (r, i) = (i < length r) andalso (i >= 0)

  (* pre: inBounds (r, i) *)
    fun subInBounds (r : 'a rope, i : int) : 'a =
	(case r
	  of Leaf s => S.sub (s, i)
	   | Cat (_, len, r1, r2) =>
             if i < length r1 then subInBounds (r1, i)
	     else subInBounds (r2, i - length r1)
	(* end case *))

  (* returns the ith element of the rope r. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
    fun sub (r : 'a rope, i : int) : 'a = 
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

  (* takes a rope length, and returns a rope balancer *)
    fun mkInitialBalancer (len : int) : 'a balancer = let
      val blen = balancerLen len
      fun initEntry n = (fib (n+2), fib (n+3), NONE)
      in
        List.tabulate (blen, initEntry)
      end 

    fun leftmostLeaf r =
	(case r
	  of Leaf _ => r
	   | Cat (_, _, rL, _) => leftmostLeaf rL
	(* end case *))

    fun rightmostLeaf r =
	(case r
	  of Leaf _ => r
	   | Cat (_, _, _, rR) => rightmostLeaf rR
	(* end case *))

  (* absorb the given sequence in the rightmost leaf of the rope *)
  (* pre: the rightmost leaf of the rope can accommodate the sequence *)
    fun absorbLeft (s : 'a seq, r : 'a rope) : 'a rope = let
      val slen = S.length s
      fun build r =
	  (case r
	    of Cat (d, len, r1, r2) => Cat (d, len+slen, build r1, r2)
	     | Leaf s' => Leaf (S.append (s, s'))
	  (* end case *))
      in
        build r
      end

  (* absorb the given sequence in the leftmost leaf of the rope *)
  (* pre: the leftmost leaf of the rope can accommodate the sequence *)
    fun absorbRight (r : 'a rope, s : 'a seq) : 'a rope = let
      val slen = S.length s
      fun build rp = (case rp
		       of Cat (d, len, r1, r2) => Cat (d, len+slen, r1, build r2)
			| Leaf s' => Leaf (S.append (s', s))
		     (* end case *))
      in
        build r
      end

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
 
  (* Concatenate all ropes in the balancer into one balanced rope. *)
    fun balToRope (balancer : 'a balancer) : 'a rope = let
      fun f (b, acc) = 
       (case b
	  of (_, _, NONE) => acc
	   | (_, _, SOME r) => appendWithoutBalancing (r, acc)
          (* end case *))
      in
        List.foldl f (mkLeaf (S.empty ())) balancer
      end

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

  (* takes a rope and returns the list of leaves in left-to-right order *)
    fun leaves (r : 'a rope) : 'a rope list = 
	(case r
	  of Leaf _ => [r]
	   | Cat (_, _, r1, r2) => leaves r1 @ leaves r2
	(* end case *))

  (* Balance a rope to within 2 of ideal depth. *)
  (* This operation is O(n*log n) in the number of leaves *)
    fun balance (r : 'a rope) : 'a rope = 
      balToRope (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary (r : 'a rope) : 'a rope = if isBalanced r then r else balance r

  (* ***** ROPE CONSTRUCTION ***** *)

  (* concatenates two ropes (with balancing) *)
    fun appendWithBalancing (r1 , r2) = balanceIfNecessary (appendWithoutBalancing (r1, r2))

  (* append (r1, r2) *)
  (* returns the rope that is the concatenation of r1 and r2 *)
    val append : 'a rope * 'a rope -> 'a rope = appendWithBalancing

  (* concatWithoutBalancing l *)
  (* returns the rope that is the concatenation of all the ropes in the list l *)
    fun concatWithoutBalancing (l : 'a rope list) : 'a rope = 
      List.foldl appendWithoutBalancing (empty ()) (List.rev l)

  (* concat l *)
  (* returns the rope that is the concatenation of all the ropes in the list l *)
    fun concat (l : 'a rope list) : 'a rope = 
	balanceIfNecessary (concatWithoutBalancing l)

  (* returns the concatenation of rs. balancing is performed w.r.t. the ropes of rs. note that the whole *)
  (* result rope is may not be balanced. *)
    fun ropeOfRopes rs = raise Fail "broken"

    structure Z1 = Zipper1Fn (
		      structure RTy = RTy
		      val empty = empty
		      val append = appendWithoutBalancing
		      val ropeOfRopes = ropeOfRopes)

    structure Z2 = Zipper2Fn (
		       structure S = S
		       structure RTy = RTy
		       val empty = empty
		       val append = appendWithoutBalancing
		       val ropeOfRopes = ropeOfRopes)

    fun updateInBounds (r, i, x) =
	let
	    fun upd (loc, i) = 
		(case loc
		  of (Leaf s, c) => (Leaf (S.update (s, i, x)), c)
		   | _ =>
		     let
			 val lloc = Option.valOf (Z1.left loc)
			 val (r1, _) = lloc
			 val l1 = length r1
		     in
			 if i < l1 then upd (lloc, i) else upd (Option.valOf (Z1.right loc), i - l1)
		     end
		(* end case *))
	in
	    Z1.output (upd (Z1.input r, i))
	end

    fun update (r, i, x) =
	if inBounds (r, i)
	then updateInBounds (r, i, x)
	else raise Subscript

    fun deleteInBounds (r, i) =
	let
	    fun del (loc, i) = 
		(case loc
		  of (Leaf s, c) => (Leaf (S.delete (s, i)), c)
		   | _ =>
		     let
			 val lloc = Option.valOf (Z1.left loc)
			 val (r1, _) = lloc
			 val l1 = length r1
		     in
			 if i < l1 then del (lloc, i) else del (Option.valOf (Z1.right loc), i - l1)
		     end
		(* end case *))
	in
	    Z1.output (del (Z1.input r, i))
	end

    fun delete (r, i) =
	if inBounds (r, i)
	then deleteInBounds (r, i)
	else raise Subscript

  (* creates a new sequence from a rope of elements *)
    fun toSeq (r : 'a rope) : 'a S.seq =
	(case r
	  of Leaf s => s
	   | Cat (_, _, rL, rR) => S.append (toSeq rL, toSeq rR)
	(* end case *))

  (* rev r *)
  (* returns a rope consisting of r's elements in reverse. *)
  (* pre: the input is balanced *)
  (* post: the output is balanced *)
    fun rev (Leaf s) = mkLeaf (S.rev s)
      | rev (Cat (d, len, r1, r2)) = Cat (d, len, rev r2, rev r1)

  (* Given a list, construct a balanced rope. *)
    fun fromList xs =
	let
	    fun chunkify (xs, len, acc) =
		if len < 1 then List.rev acc
		else let
			val sz = Int.min (maxLeafSize, len)
			val (xs', nxt) = (List.drop (xs, sz), List.take (xs, sz))
		    in
			chunkify (xs', len - sz, nxt :: acc)
		    end
	    val len = List.length xs
	    val chunks = chunkify (xs, len, nil)
	    fun leafFromList xs = mkLeaf (S.fromList xs)
	in
	    balToRope (List.foldl insert (mkInitialBalancer len) (List.map leafFromList chunks))
	end

  (* creates a new rope from a sequence of elements. *)
    fun fromSeq (s : 'a S.seq) : 'a rope = fromList (S.toList s)

  (* creates a new rope of characters from a string *)
    val fromString : string -> char rope = fromList o String.explode

  (* ***** ROPE DECONSTRUCTION ***** *)

  (* pre: inBounds(r, i) *)
    fun splitAtWithoutBalancing (r : 'a rope, i : int) : 'a rope * 'a rope  = 
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

  (* pre: inBounds (r, i) *)
    fun splitAtWithBalancing (r, i) = let
      val (r1, r2) = splitAtWithoutBalancing (r, i)
      in
	RT.forkjoin (fn () => balanceIfNecessary r1, fn () => balanceIfNecessary r2)
      end

  (* returns two ropes r1 and r2 such that r1 contains the first i+1 elements of the r and r2 contains *)
  (* the rest of the elements of r. r1 and r2 have the same order as r *)
  (* If i < 0 or length r <= i, then the Subscript exception is raised *)
    fun splitAt (r : 'a rope, i : int) : 'a rope * 'a rope =
      if inBounds(r, i)
      then splitAtWithBalancing(r, i)
      else raise Subscript

  (* cut (r, n) *)
  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r] *)
    fun cut (r, n) =
	  if n = 0 then (mkLeaf (S.empty ()), r)
	  else splitAt(r, n - 1)

  (* take (r, n) *)
  (* returns the first i elements of the rope r. Raises Subscript if i < 0 or i >= length r. *)
    fun take (r : 'a rope, n : int) : 'a rope = #1(cut(r, n))

  (* drop (r, n) *)
  (* returns what is left after dropping the first i elements of the rope. Raises Subscript if i < 0 *)
  (* or i > length r. It holds that append (take(r, i), drop(r, i)) = r when 0 <= i <= length r. *)
    fun drop (r : 'a rope, n : int) : 'a rope = #2(cut(r, n))

  (* creates a new list from a rope of elements *)
    fun toList (r : 'a rope) : 'a list = let
      fun build r =
	  (case r
	    of Leaf s => S.toList s
	     | Cat (_, _, rL, rR) => build rL @ build rR
	  (* end case *))
      in
        build r
      end

  (* binarySplitWithoutBalancing r *)
  (* split the rope r into two subropes. each subrope has size <= ceiling(|r|/2) *)
    fun binarySplitWithoutBalancing r = 
	if length r < 2 then (r, empty ())
	else splitAtWithoutBalancing (r, length r div 2 - 1)

  (* narySplitWithoutBalancing (r, k) *)
  (* split the rope r into MIN(length r, k) subropes; each subrope has roughly the same length *)
  (* pre: k > 0 *)
  (* NOTE: the subropes may be unbalanced *)
    fun narySplitWithoutBalancing (r, k) =
	let val minc = if length r <= k then 1 else length r div k  (* min subrope size *)
	    val maxc = Int.max (length r div k + length r mod k, minc) (* max subrope size *)
	    fun lp (r, rps) =
		if length r <= maxc then List.rev (r :: rps)
		else let val (r1, r2) = splitAtWithoutBalancing (r, minc - 1)
		     in
			 lp (r2, r1 :: rps)
		     end
	in
	    if length r <= k then List.map (mkLeaf o S.singleton) (toList r)
	    else if k > 0 then lp (r, nil)
	    else raise Fail "narySplitWithoutBalancing: k < 1"
	end

    local

	fun nsmap f r = 
	    let fun m rp = 
		    (case rp
		      of Leaf s => mkLeaf (S.map f s)
		       | Cat (d, len, r1, r2) => Cat (d, len, m r1, m r2)
		    (* end case *))
	    in
		m r
	    end

	fun ebsspmap SST f rp =
	    let fun m rp =
		    if length rp <= SST then nsmap f rp
		    else
			let val (rp1, rp2) = binarySplitWithoutBalancing rp
			in
			    appendWithoutBalancing (RT.forkjoin (fn () => m rp1,
								 fn () => m rp2))
			end
	    in
		m rp
	    end

	fun topdownmap K f rp =
	    concatWithoutBalancing (RT.parMap (nsmap f) (narySplitWithoutBalancing (rp, K * RT.numAvailProcs ())))

	fun ebsapmap K V f rp =
	    let
		fun m (rp, n, wid) =
		    if length rp = 0 then empty ()
		    else if length rp = 1 then singleton (f (sub (rp, 0)))
		    else if n <= 1 then nsmap f rp
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				m (rp, Int.max (n, V), curWid)
			    else
				let
				    val (rp1, rp2) = binarySplitWithoutBalancing rp
				    val n' = n div 2
				in
				    appendWithoutBalancing (RT.forkjoin (fn () => m (rp1, n', curWid),
									 fn () => m (rp2, n', curWid)))
								      
				end
			end
	    in
		m (rp, K * RT.numAvailProcs (), RT.workerId ())
	    end

      (* mapUntil k cond f rp *)
      (* returns either the result of mapping f over rp or, if cond () returns true the pair (unprd, prd). *)
      (* - unprd records the elements yet to be processed and prd processed ones *)
      (* - k is the number of elements to process before checking the condition *)
      (* pre: k > 0 *)
	fun mapUntil k cond f rp =
	    let fun dfs loc =
		    let val (LEFT (Leaf s), c) = Z2.leftmost loc
		    in
			case S.mapUntil k cond f s
			 of PARTIAL (unprd, prd) => PARTIAL (Z2.split (unprd, prd, c))
			  | COMPLETE s' => (case Z2.upUntilLThenRight (RIGHT (Leaf s'), c)
					     of NONE => COMPLETE (Z2.output (RIGHT (Leaf s'), c))
					      | SOME loc' => dfs loc')
		    end
	    in
		dfs (Z2.input rp)
	    end

	fun lbsmap PPT f rp = 
	    let fun m rp = 
		    if length rp = 0 then empty ()
		    else if length rp = 1 then singleton (f (sub (rp, 0)))
		    else
			(case mapUntil PPT RT.otherHungryProcs f rp
			  of PARTIAL (unprd, prd) =>
			     let 
				 val (unprd1, unprd2) = binarySplitWithoutBalancing unprd
			     in
				 appendWithoutBalancing (prd, 
				   appendWithoutBalancing (
				     RT.forkjoin (fn () => m unprd1, fn () => m unprd2)))
			     end
			   | COMPLETE rp => balanceIfNecessary rp
		    (* end case *))
	    in
		m rp
	    end
			       
	fun lpsmap PPT f rp = 
	    let fun m rp =
		    if length rp = 0 then empty ()
		    else if length rp = 1 then singleton (f (sub (rp, 0)))
		    else
			(case mapUntil PPT RT.otherHungryProcs f rp
			  of PARTIAL (unprd, prd) =>
			     appendWithoutBalancing (prd, 
			       concatWithoutBalancing (RT.parMap m (narySplitWithoutBalancing (unprd, RT.numAvailProcs ()))))
			   | COMPLETE rp => balanceIfNecessary rp)
	    in
		m rp
	    end

    in

    (* map f r *)
    (* applies f to each element of r from left to right, returning the list of results. *)
    fun map f r =
	(case RT.splittingStrategy
	  of RT.NO_SPLIT => nsmap f r
	   | RT.EBS_SP {SST} => ebsspmap SST f r
	   | RT.EBS_AP {K, V} => ebsapmap K V f r
	   | RT.LBS {PPT} => lbsmap PPT f r
	   | RT.LPS {PPT} => lpsmap PPT f r
	(* end case *))

    end (* local *)

  (* mapPartial f r *)
  (* applies f to each element of r from left to right, returning a rope of results where f *)
  (* was defined. f is not defined for an element of r if f applied to the element returns NONE. *)
    fun mapPartial (f : 'a -> 'b option) : 'a rope -> 'b rope = let
      fun m (Leaf s) = let
            val s' = S.mapPartial f s
            in
	      mkLeaf s'
            end
	| m (Cat (_, _, rL, rR)) = appendWithoutBalancing (m rL, m rR)
      in
        balanceIfNecessary o m
      end

    local

	fun nsfilter pred r = 
	    let fun filt r =
		    (case r
		      of Leaf s => mkLeaf (S.filter pred s)
		       | Cat (_, _, r1, r2) =>
			 appendWithoutBalancing (filt r1, filt r2)
		    (* end case *))
	    in
		balanceIfNecessary (filt r)
	    end

	fun ebsspfilter SST pred r =
	    let fun filt r =
		    if length r <= SST then nsfilter pred r
		    else
			let val (r1, r2) = binarySplitWithoutBalancing r
			in
			    appendWithoutBalancing (RT.forkjoin (fn () => filt r1,
							      fn () => filt r2))
			end
	    in
		balanceIfNecessary (filt r)
	    end

	fun ebsapfilter K V pred rp = 
	    let
		fun flt (rp, n, wid) =
		    if length rp <= 1 orelse n <= 1 then nsfilter pred rp
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				flt (rp, Int.max (n, V), curWid)
			    else
				let
				    val (rp1, rp2) = binarySplitWithoutBalancing rp
				    val n' = n div 2
				in
				    appendWithoutBalancing (RT.forkjoin (fn () => flt (rp1, n', curWid),
									 fn () => flt (rp2, n', curWid)))
								      
				end

			end
	    in
		flt (rp, K * RT.numAvailProcs (), RT.workerId ())
	    end

	fun topdownfilter K pred r =concatWithoutBalancing (RT.parMap (nsfilter pred) (narySplitWithoutBalancing (r, K * RT.numAvailProcs ())))

      (* filterUntil k cond pred rp *)
      (* returns either the result of filtering rp by pred or, if cond () returns true the pair (unprd, prd) *)
      (* - unprd records the unprocessed elements and prd is the processed ones *)
      (* - k is the number of elements to process before checking the condition *)
      (* pre: k > 0 *)
	fun filterUntil k cond f rp =
	    let fun flp loc =
		    let val (Leaf s, c) = Z1.leftmost loc
		    in
			case S.filterUntil k cond f s
			 of PARTIAL (unprd, prd) => PARTIAL (Z1.split (unprd, prd, c))
			  | COMPLETE s' => (case Z1.upUntilLThenRight (Leaf s', c)
					  of NONE => COMPLETE (Z1.output (Leaf s', c))
					   | SOME loc' => flp loc')
		    end
	    in
		flp (Z1.input rp)
	    end

	fun lbsfilter PPT pred r = 
	    let fun filt r =
		    if length r = 0 then empty ()
		    else if length r = 1 then 
			let
			    val x = sub (r, 0)
			in
			    if pred x then singleton x else empty ()
			end
		    else
			(case filterUntil PPT RT.otherHungryProcs pred r
			  of PARTIAL (unprd, prd) =>
			     let
				 val (unprd1, unprd2) = binarySplitWithoutBalancing unprd
			     in
				 appendWithoutBalancing (
			           RT.forkjoin (fn () => balanceIfNecessary prd, 
						fn () => appendWithoutBalancing (
						  RT.forkjoin (fn () => balanceIfNecessary (filt unprd1),
							       fn () => balanceIfNecessary (filt unprd2)))))
			     end
			   | COMPLETE r' => balanceIfNecessary r'
			(* end case *))
	    in
		filt r
	    end

	fun lpsfilter PPT pred r = 
	    let fun filt r =
		    if length r = 0 then empty ()
		    else if length r = 1 then 
			let
			    val x = sub (r, 0)
			in
			    if pred x then singleton x else empty ()
			end
		    else
			(case filterUntil PPT RT.otherHungryProcs pred r
			  of PARTIAL (unprd, prd) =>
			     appendWithoutBalancing (balanceIfNecessary prd, 
			       concat (RT.parMap (balanceIfNecessary o filt) 
						 (narySplitWithoutBalancing (unprd, RT.numAvailProcs ()))))
			   | COMPLETE r' => balanceIfNecessary r'
			(* end case *))
	    in
		filt r
	    end

    in

      (* filter pred r *)
      (* applies pred to each element x of r, from left to right, and returns the rope of those x for which *)
      (* pred x evaluated to true, in the same order as the occurred in the argument rope. *)
      (* post: the output is balanced *)
      (* Strategy: First, filter all the ropes and append without balancing. *)
      (*           Then balance the whole thing if needed. *)
	fun filter pred r =
	    (case RT.splittingStrategy
	      of RT.NO_SPLIT => nsfilter pred r
	       | RT.EBS_SP {SST} => ebsspfilter SST pred r
	       | RT.EBS_AP {K, V} => ebsapfilter K V pred r
	       | RT.LBS {PPT} => lbsfilter PPT pred r
	       | RT.LPS {PPT} => lpsfilter PPT pred r
	    (* end case *))

    end (* local *)

  (* foldl f z r *)
  (*
     returns
	    f(rn,...,f(r2, f(r1, z))...)
     or z if the list is empty. 
   *)
    fun foldl f z r = let
      fun fdl (rp, acc) = 
	  (case rp
	    of Leaf s => S.foldl f acc s
	     | Cat (_, _, rL, rR) => fdl (rR, fdl (rL, acc))
	  (* end case *))
      in
        fdl (r, z)
      end

  (* foldli f z r *)
  (*
     returns
	    f(n-1, rn,...,f(1, r2, f(0, r1, z))...)
     or z if the list is empty. 
   *)
    fun foldli f z r = let
      fun f' (x, (i, acc)) = (i+1, f (i, x, acc))
      val (_, res) = foldl f' (0, z) r
      in
        res
      end

  (* foldr f z r *)
  (*
     returns
	    f(r1, f(r2, ..., f(rn, z)...))
     or b if the list is empty. 
   *)
    fun foldr f z rp =
	(case rp
	  of Leaf s => S.foldr f z s
	   | Cat (_, _, rL, rR) => foldr f (foldr f z rR) rL
	(* end case *))

  (* foldri f z r *)
  (*
     returns
	    f(0, r1, f(1, r2, ..., f(n-1, rn, z)...))
     or b if the list is empty. 
   *)
    fun foldri f z r = let
      fun f' (x, (i, acc)) = (i-1, f (i, x, acc))
      val (_, res) = foldr f' (length r - 1, z) r
      in
	res
      end

    local

	fun ebsspreduce SST aop z rp =
	    let fun red rp =
		    if length rp <= SST then foldl aop z rp
		    else
			let val (rp1, rp2) = binarySplitWithoutBalancing rp
			in
			    aop (RT.forkjoin (fn () => red rp1, fn () => red rp2))
			end
	    in
		red rp
	    end

	fun topdownreduce K V aop z rp =
	    List.foldl aop z (RT.parMap (foldl aop z) (narySplitWithoutBalancing (rp, K * RT.numAvailProcs ())))

	fun ebsapreduce K V aop z rp =
	    let
		fun red (rp, n, wid) =
		    if length rp = 0 then z
		    else if length rp = 1 then sub (rp, 0)
		    else if n <= 1 then foldl aop z rp
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				red (rp, Int.max (n, V), curWid)
			    else
				let
				    val (rp1, rp2) = binarySplitWithoutBalancing rp
				    val n' = n div 2
				in
				    aop (RT.forkjoin (fn () => red (rp1, n', curWid),
						      fn () => red (rp2, n', curWid)))
								      
				end
			end
	    in
		red (rp, K * RT.numAvailProcs (), RT.workerId ())
	    end

      (* reduceUntil k cond aop z rp *)
      (* returns either the result of mapping f over rp or, if cond () returns true the pair (unprd, red). *)
      (* - unprd records the elements yet to be processed and prd the result of the reduction so far *)
      (* - k is the number of elements to process before checking the condition *)
      (* pre: k > 0 *)
	fun reduceUntil k cond aop z rp =
	    let fun red (loc, acc) =
		    let val (Leaf s, c) = Z1.leftmost loc
		    in
			case S.reduceUntil k cond aop z s
			 of PARTIAL ((unprd, _), acc') => 
			    PARTIAL (Z1.output (Leaf unprd, c), aop (acc, acc'))
			  | COMPLETE acc' => (case Z1.upUntilLThenRight (empty (), c)
					    of NONE => COMPLETE (aop (acc, acc'))
					     | SOME loc' => red (loc', aop (acc, acc'))
					  (* end case *))
		        (* end case *)
		    end
	    in
		red (Z1.input rp, z)
	    end

	fun lbsreduce PPT aop z rp =
	    let fun red rp =
		    if length rp = 0 then z
		    else if length rp = 1 then aop (z, sub (rp, 0))
		    else
			(case reduceUntil PPT RT.otherHungryProcs aop z rp
			  of PARTIAL (unprd, acc) =>
			     let 
				 val (unprdL, unprdR) = binarySplitWithoutBalancing unprd
			     in
				 aop (acc, aop (RT.forkjoin (fn () => red unprdL, fn () => red unprdR)))
			     end
			   | COMPLETE acc => acc
			(* end case *))
	    in
		red rp
	    end

	fun lpsreduce PPT aop z rp = 
	    let fun red rp =
		    if length rp = 0 then z
		    else if length rp = 1 then aop (z, sub (rp, 0))
		    else
			(case reduceUntil PPT RT.otherHungryProcs aop z rp
			  of PARTIAL (unprd, acc) =>
			     let val unprds = narySplitWithoutBalancing (unprd, RT.numAvailProcs ())
			     in
				 List.foldl aop acc (RT.parMap red unprds)
			     end
			   | COMPLETE acc => acc
			(* end case *))
	    in
		red rp
	    end

    in
  (* reduce aop z r *)
  (* returns the reduction z aop r_0 aop r_1 ... aop r_n over the rope r *)
  (* pre: aop is an associative operator *)
    fun reduce aop z r =
	(case RT.splittingStrategy
	  of RT.NO_SPLIT => foldl aop z r
	   | RT.EBS_SP {SST} => ebsspreduce SST aop z r
	   | RT.EBS_AP {K, V} => ebsapreduce K V aop z r
	   | RT.LBS {PPT} => lbsreduce PPT aop z r
	   | RT.LPS {PPT} => lpsreduce PPT aop z r
	(* end case *))
    end (* local *)

    local

	fun seqtab f (lo, hi) = S.tabulate (hi - lo + 1, fn i => f (lo + i))

	fun intervalLength (lo, hi) = hi - lo + 1

      (* pre: intervalLength (lo, hi) > 1 *)				
	fun binarySplitInterval (lo, hi) = 
	    let 
		val m = (intervalLength (lo, hi)) div 2 + lo
	    in
		((lo, m - 1), (m, hi))
	    end
	
	fun nstabulate f (lo, hi) =
	    let fun tab (lo, hi) =
		    let 
			val len = intervalLength (lo, hi)
		    in
			if len <= maxLeafSize orelse len < 2 then mkLeaf (seqtab f (lo, hi))
			else 
			    let
				val (interval1, interval2) = binarySplitInterval (lo, hi)
			    in
				appendWithoutBalancing (tab interval1, tab interval2)
			    end
		    end
	    in
		tab (lo, hi)
	    end

	fun ebssptabulate SST (n, f) =
	    let fun tab (lo, hi) =
		    let val len = hi - lo + 1
		    in
			if len <= SST orelse len < 2 then nstabulate f (lo, hi)
			else let val m = len div 2 + lo
			     in
				 appendWithoutBalancing (RT.forkjoin (fn () => tab (lo, m - 1),
								      fn () => tab (m, hi)))
			     end
		    end
	    in
		tab (0, n - 1)
	    end

      (* returns at most nChunks intervals over the iteration space of n in which each interval has at most *)
      (* chunkSize iterations *)
      (* pre: n > nChunks andalso n > chunkSize *)
      (* post: nChunks <= List.length (mkIntervals (n, nDesired)) <= nChunks + 1 *)
      (* e.g., mkIntervals (1, 1) ==> [(0, 0)] *)
      (* e.g., mkIntervals (3, 2) ==> [(0, 0), (1, 1), (2, 2)] *)
      (* e.g., mkIntervals (7, 3) ==> [(0,1),(2,3),(4,5),(6,6)] *)
	fun mkIntervals (n, nDesired) =
	    let fun iter (i, xs, acc) =
		    (case xs
		      of nil => List.rev acc
		       | x :: xs => iter (i + x, xs, (i, i + x - 1) :: acc)
		    (* end case *))
		val xs =
		    if n <= nDesired then n :: nil
		    else if n mod nDesired = 0 then List.tabulate (nDesired, fn _ => n div nDesired)
		    else List.tabulate (nDesired, fn i => n div nDesired) @ [n mod nDesired]
	    in
		iter (0, xs, nil)
	    end

	fun topdowntabulate K V (n, f) =
	    let val nChunks = Int.max (Int.min (K * RT.numAvailProcs (), n), 1)
	    in
		if nChunks = 1 then nstabulate f (0, n - 1)
		else concat (RT.parMap (nstabulate f) (mkIntervals (n, nChunks)))
	    end

	fun ebsaptabulate K V (n, f) =
	    let
		fun tab (interval, n, wid) =
		    if intervalLength interval <= 1 orelse n <= 1 then nstabulate f interval
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				tab (interval, Int.max (n, V), curWid)
			    else
				let
				    val (interval1, interval2) = binarySplitInterval interval
				    val n' = n div 2
				in
				    appendWithoutBalancing (RT.forkjoin (fn () => tab (interval1, n', curWid),
									 fn () => tab (interval2, n', curWid)))
								      
				end

			end
	    in
		tab ((0, n - 1), K * RT.numAvailProcs (), RT.workerId ())
	    end

	fun tabulateUntil k cond (lo, hi, f) =
	    let fun tab (lo, hi, acc) =
		    let val len = Int.min (hi - lo + 1, maxLeafSize)
		    in
			if len < 1 then COMPLETE (concat (List.rev acc))
			else (case S.tabulateUntil k cond (lo, lo + len - 1, f)
			       of PARTIAL (_, prd) => PARTIAL ((), concat (List.rev (Leaf prd :: acc)))
				| COMPLETE s => tab (lo + len, hi, mkLeaf s :: acc)
			     (* end case *))
		    end
	    in
		tab (lo, hi, nil)
	    end

	fun lbstabulate PPT (n, f) =
	    let fun tab (lo, hi) =
		    let 
			val len = hi - lo + 1
		    in
			if len < 1 then empty ()
			else if len = 1 then singleton (f lo)
			else (case tabulateUntil PPT RT.otherHungryProcs (lo, hi, f)
			       of PARTIAL (_, prd) =>
				  let val prdLen = length prd
				      val lo' = prdLen + lo
				      val len' = hi - lo' + 1
				      val m = len' div 2 + lo'
				  in
				      appendWithoutBalancing (prd,
					    appendWithoutBalancing (RT.forkjoin (fn () => tab (lo', m - 1), 
										 fn () => tab (m, hi))))
				  end
				| COMPLETE rp => rp
			     (* end case *))
		    end
	    in
		tab (0, n - 1)
	    end

	fun lpstabulate PPT (n, f) =
	    let fun tab (lo, hi) =
		    let 
			val len = hi - lo + 1
		    in
			if len < 1 then empty ()
			else if len = 1 then singleton (f lo)
			else (case tabulateUntil PPT RT.otherHungryProcs (lo, hi, f)
			       of PARTIAL (_, prd) =>
				  let val prdLen = length prd
				      val lo' = prdLen + lo
				      val len' = hi - lo' + 1
				      val nChunks = Int.max (Int.min (RT.numAvailProcs (), len'), 1)
				      val chunks = List.map (fn (lo'', hi'') => (lo'' + lo', hi'' + lo')) 
							    (mkIntervals (len', nChunks))
				  in				      
				      concat (prd :: RT.parMap tab chunks)
				  end
				| COMPLETE rp => rp
			     (* end case *))
		    end
	    in
		tab (0, n - 1)
	    end

    in

  (* creates a rope of n elements, where the elements are defined in order of increasing index by applying *)
  (* f to the element's index. This is equivalent to the expression: *)
  (*   fromList (List.tabulate (n, f)) *)
    fun tabulate (n, f) =
	(case RT.splittingStrategy
	  of RT.NO_SPLIT => nstabulate f (0, n - 1)
	   | RT.EBS_SP {SST} => ebssptabulate SST (n, f)
	   | RT.EBS_AP {K, V} => ebsaptabulate K V (n, f)
	   | RT.LBS {PPT} => lbstabulate PPT (n, f)
	   | RT.LPS {PPT} => lpstabulate PPT (n, f)
	(* end case *))

    end

  (* partition f r *)
  (* applies f to each element x of r, from left to right, and returns a pair (pos, neg) where pos is *)
  (* the rope of those x for which f x evaluated to true, and neg is the rope of those for which f x *)
  (* evaluated to false. The elements of pos and neg retain the same relative order they possessed in r. *) 
    fun partition (pred : 'a -> bool) (r : 'a rope) : 'a rope * 'a rope = 
	RT.forkjoin (fn () => filter pred r, fn () => filter (not o pred) r)

  (* find f r *)
  (* applies f to each element x of the rope r, from left to right, until f x evaluates to true; *)
  (* returns SOME x if such an x exists, otherwise NONE. *)
    fun find (pred : 'a -> bool) (r : 'a rope) : 'a option = let
      fun find rp = 
	  (case rp
	    of Leaf s => S.find pred s
	     | Cat (_, _, rL, rR) =>
               (case find rL
		 of NONE => find rR
		  | something => something
	       (* end case *))
	  (* end case *))
      in
	find r
      end

  (* exists f r *)
  (* applies f to each element x of the rope r, from left to right, until f x evaluates to true; *)
  (* returns true if such an x exists and false otherwise. *)
(* TODO: short-circuiting version of exists *)
  fun exists pred r = reduce (fn (x, y) => x orelse y) false (map pred r)

  (* all f r *)
  (* applies f to each element x of the rope r, from left to right, until f x evaluates to false; *)
  (* returns false if such an x exists and true otherwise. Equivalent to not(exists (not o f) r)). *)
(* TODO: short-circuiting version of all *)
  fun all pred r = reduce (fn (x, y) => x andalso y) true (map pred r)

  (* app f r *)
  (* applies f to the elements of l, from left to right. *)
    fun app f r = let
      fun go rp =
	  (case rp
	    of Leaf s => S.app f s
	     | Cat (_, _, rL, rR) => (go rL; go rR)
	  (* end case *))
      in 
        go r 
      end

  (* appi f r *)
  (* applies f to the elements of l, from left to right. *)
    fun appi f r = let
      val i = ref 0
      fun f' x = (f (!i, x); i := !i + 1)
      fun go rp =
	  (case rp
	    of Leaf s => S.app f' s
	     | Cat (_, _, rL, rR) => (go rL; go rR)
	  (* end case *))
      in 
        go r 
      end

    val r = Random.rand (~2343333, 2343)
    fun roll n = Random.randNat r mod n

  (* randomRope (f, maxDepth, leafSize) *)
  (* returns a randomly generated rope of a maximum depth and a fixed leaf size. each element *)
  (* is generated from an application of f. *)
    fun randomRope (f, maxDepth, leafSize) =
	let val leafSize = Int.min (leafSize, maxLeafSize)
	    fun build d =
		if roll 5 <= 3 andalso d < maxDepth then
		    appendWithoutBalancing (build (d + 1), build (d + 1))
		else
		    Leaf (S.tabulate (leafSize, fn _ => f ()))
	in
	    build 0
	end

    structure Pair = RopePairFn (
			 structure RTy = RTy
			 structure RT = RT
			 val concat = concat
			 val append = appendWithoutBalancing
			 val empty = empty
			 val singleton = singleton
			 val length = length
			 val binarySplit = binarySplitWithoutBalancing
			 val narySplit = narySplitWithoutBalancing
			 val sub = sub
			 val ropeOfRopes = ropeOfRopes
			 val balanceIfNecessary = balanceIfNecessary)

    structure Scan = RopeScanFn (
			     structure S = S
			     structure RTy = RTy
			     structure RT = RT
			     val empty = empty
			     val singleton = singleton
			     val length = length
			     val depth = depth
			     val splitAt = splitAt
			     val binarySplit = binarySplitWithoutBalancing
			     val narySplit = narySplitWithoutBalancing
			     val append = appendWithoutBalancing
			     val concat = concat
			     val reduce = reduce
			     val sub = sub
			     val balanceIfNecessary = balanceIfNecessary)

    structure Permute = RopePermuteFn (
			    structure RTy = RTy
			    structure RT = RT
			    val maxLeafSize = maxLeafSize
			    val sub = sub
			    val length = length
			    val splitAt = splitAt
			    val partition = partition
			    val append = append
			    val map = map
			    val tabulate = tabulate
			    val app = app
			    val singleton = singleton
			    val delete = delete)

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
