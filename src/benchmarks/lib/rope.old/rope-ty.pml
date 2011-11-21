structure RopeTy =
  struct

    structure S = VectorSeq

    type 'a seq = 'a S.seq

    datatype 'a rope
      = Cat of (int *         (* depth *)
		int *         (* length *)
		'a rope *     (* left subtree *)
		'a rope       (* right subtree *))
      | Leaf of 'a seq        (* sequence *)

  (* Get max leaf size <size> from command-line argument:
   *   -max-leaf-size <size>
   * If no such argument is provided, use the default max leaf size of 256.
   *)
    local
	val dflt = 256
	fun stringSame (s1, s2) = String.same (s1, s2)
	fun getMaxLeafSize ss = 
	    (case ss
	      of s1 :: s2 :: ss =>
		 if stringSame (s1, "-max-leaf-size") then Option.getOpt (Int.fromString s2, dflt)
		 else getMaxLeafSize (s2 :: ss)
	       | _ => dflt)
    in
    val maxLeafSize = getMaxLeafSize (CommandLine.arguments ())
    end

  (* failwith msg *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

  (* this constructor maintains the invariant that the leaf size must be *)
  (* <= to the constant maxLeafSize *)
    fun mkLeaf seq =
	  if S.length seq > maxLeafSize 
	    then failwith "RopeFn.mkLeaf: bogus leaf size"
	  else Leaf seq

    fun empty () = Leaf (S.empty ())

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
    fun absorbLeft (s, r) = let
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
    fun absorbRight (r, s) = let
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
    fun appendWithoutBalancing (r1, r2) =
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

  (* Is the given int a valid index of the given rope? *)
    fun inBounds (r, i) = (i < length r) andalso (i >= 0)

  (* pre: inBounds (r, i) *)
    fun subInBounds (r, i) =
	(case r
	  of Leaf s => S.sub (s, i)
	   | Cat (_, len, r1, r2) =>
             if i < length r1 then subInBounds (r1, i)
	     else subInBounds (r2, i - length r1)
	(* end case *))

  (* returns the ith element of the rope r. If i < 0 or length r <= i, then the Subscript *)
  (* exception is raised. *)
    fun sub (r, i) = 
      if inBounds (r, i) 
      then subInBounds (r, i)
      else failwith "subscript"

  (* returns a string displaying the internal structure of the given rope *)
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

  (* ***** BALANCING ***** *)

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
    fun mkInitialBalancer len = let
      val blen = balancerLen len
      fun initEntry n = (fib (n+2), fib (n+3), NONE)
      in
        List.tabulate (blen, initEntry)
      end

  (* Insert a rope into a balancer. *)
  (* invariant: the length of the rope at position i is in its interval, that is, *)
  (*   greater than or equal to the lower bound, and less that the upper bound. *)
  (* See \cite{bap:ropes} for details. *)
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
             insert (appendWithoutBalancing (r', r), (lb, ub, NONE) :: t)
        (* end case *))

  (* Concatenate all ropes in the balancer into one balanced rope. *)
    fun balToRope balancer = let
      fun f (b, acc) = 
       (case b
	  of (_, _, NONE) => acc
	   | (_, _, SOME r) => appendWithoutBalancing (r, acc)
          (* end case *))
      in
        List.foldl f (mkLeaf (S.empty ())) balancer
      end

  (* returns the concatenation of rs. balancing is performed w.r.t. the ropes of rs. note that the whole *)
  (* result rope is may not be balanced. *)
    fun ropeOfRopes rs = failwith "broken"

  end
