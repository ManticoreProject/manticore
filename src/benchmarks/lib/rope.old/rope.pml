(* rope.pml
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

structure Rope =
  struct

    structure RT = Runtime
    structure RTy = RopeTy
    structure S = RTy.S
    structure Z1 = Zipper1
    structure Z2 = Zipper2

    datatype rope = datatype RTy.rope
    datatype either = datatype Either.either
    datatype progress = datatype ProgressTy.progress

    val maxLeafSize = RTy.maxLeafSize
    val empty = RTy.empty
    val singleton = RTy.singleton
    val isEmpty = RTy.isEmpty
    val length = RTy.length
    val depth = RTy.depth
    val appendWithoutBalancing = RTy.appendWithoutBalancing
    val toString = RTy.toString
    val inBounds = RTy.inBounds
    val sub = RTy.sub
    val mkInitialBalancer = RTy.mkInitialBalancer
    val insert = RTy.insert
    val balToRope = RTy.balToRope
    val mkLeaf = RTy.mkLeaf

    val ceilLog2 = Int.ceilingLg

  (* failwith msg *)
  (* using this for the moment so we can observe the exception message at runtime *)
    fun failwith msg = (Print.printLn msg; (raise Fail msg))

  (* Split the list into two pieces. *)
  (* This function is basically take and drop at the same time. *)
  (* It doesn't raise an exn if there aren't enough elements. *)
  (* ex: split ([1,2,3], 0) => ([],[1,2,3]) *)
  (* ex: split ([1,2,3], 1) => ([1],[2,3])  *)
  (* ex: split ([1,2,3], 2) => ([1,2],[3])  *)
  (* ex: split ([1,2,3], 4) => ([1,2,3],[]) *)
    fun split (xs, n) = let
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

  (* balancing condition for ropes *)
  (* The max depth here is given in \cite{bap:ropes}. *)
    fun isBalanced rp = 
	(case rp
	  of Leaf _ => true
	   | Cat (depth, len, _, _) => (depth <= (ceilLog2 len) + 2)
	(* end case *))

  (* takes a rope and returns the list of leaves in left-to-right order *)
    fun leaves r (*: 'a rope list*) = 
	(case r
	  of Leaf _ => [r]
	   | Cat (_, _, r1, r2) => leaves r1 @ leaves r2
	(* end case *))

  (* Balance a rope to within 2 of ideal depth. *)
  (* This operation is O(n*log n) in the number of leaves *)
    fun balance r = 
      balToRope (List.foldl insert (mkInitialBalancer (length r)) (leaves r))

  (* balance a rope only when it is unbalanced *)
    fun balanceIfNecessary r = 
	if isBalanced r then r 
	else
	    let
		val len = length r
		val _ = Logging.logRopeRebalanceBegin len
		val r' = balance r
		val _ = Logging.logRopeRebalanceEnd len
	    in
		r'
	    end

  (* ***** ROPE CONSTRUCTION ***** *)

  (* concatenates two ropes (with balancing) *)
    fun appendWithBalancing (r1 , r2) = balanceIfNecessary (appendWithoutBalancing (r1, r2))

  (* append (r1, r2) *)
  (* returns the rope that is the concatenation of r1 and r2 *)
    val append = appendWithBalancing

  (* concatWithoutBalancing l *)
  (* returns the rope that is the concatenation of all the ropes in the list l *)
    fun concatWithoutBalancing l = 
      List.foldl appendWithoutBalancing (empty ()) (List.rev l)

  (* concat l *)
  (* returns the rope that is the concatenation of all the ropes in the list l *)
    fun concat l = 
	balanceIfNecessary (concatWithoutBalancing l)

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
	else failwith "update: Subscript"

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
	else failwith "delete: Subscript"

  (* creates a new sequence from a rope of elements *)
    fun toSeq r =
	(case r
	  of Leaf s => s
	   | Cat (_, _, rL, rR) => S.append (toSeq rL, toSeq rR)
	(* end case *))

    fun rev r = 
     (case r
        of Leaf s => mkLeaf (S.rev s)
	 | Cat (d, len, r1, r2) => let
	     val (r1, r2) = (| rev r1, rev r2 |)
	     in
	       Cat (d, len, r2, r1)
	     end
        (* end case *))

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
    fun fromSeq s = fromList (S.toList s)

  (* ***** ROPE DECONSTRUCTION ***** *)

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

  (* pre: inBounds (r, i) *)
    fun splitAtWithBalancing (r, i) = let
      val (r1, r2) = splitAtWithoutBalancing (r, i)
      in
        RT.forkjoin (fn () => balanceIfNecessary r1, fn () => balanceIfNecessary r2)
      end

  (* returns two ropes r1 and r2 such that r1 contains the first i+1 elements of the r and r2 contains *)
  (* the rest of the elements of r. r1 and r2 have the same order as r *)
  (* If i < 0 or length r <= i, then the Subscript exception is raised *)
    fun splitAt (r, i) =
      if inBounds(r, i)
      then splitAtWithBalancing(r, i)
      else failwith "subscript"

  (* cut (r, n) *)
  (* cut the rope r into r[0, ..., n-1] and r[n, ..., length r] *)
    fun cut (r, n) =
	  if n = 0 then (mkLeaf (S.empty ()), r)
	  else splitAt(r, n - 1)

  (* take (r, n) *)
  (* returns the first i elements of the rope r. Raises Subscript if i < 0 or i >= length r. *)
    fun take (r, n) (*: 'a rope*) = 
	let val (x, _) = cut(r, n)
	in x end

  (* drop (r, n) *)
  (* returns what is left after dropping the first i elements of the rope. Raises Subscript if i < 0 *)
  (* or i > length r. It holds that append (take(r, i), drop(r, i)) = r when 0 <= i <= length r. *)
    fun drop (r, n) =
	let val (_, y) = cut (r, n)
	in y end

  (* creates a new list from a rope of elements *)
    fun toList r = let
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
	    fun mk x = mkLeaf (S.singleton x)
	in
	    if length r <= k then List.map mk (toList r)
	    else if k > 0 then lp (r, nil)
	    else raise Fail "narySplitWithoutBalancing: k < 1"
	end

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
	   | RT.EBS_SP SST => ebsspreduce SST aop z r
	   | RT.EBS_AP (K, V) => ebsapreduce K V aop z r
	   | RT.LBS PPT => lbsreduce PPT aop z r
	   | RT.LPS PPT => lpsreduce PPT aop z r
	(* end case *))

    fun reduce' ss aop z r =
	(case ss
	  of RT.NO_SPLIT => foldl aop z r
	   | RT.EBS_SP SST => ebsspreduce SST aop z r
	   | RT.EBS_AP (K, V) => ebsapreduce K V aop z r
	   | RT.LBS PPT => lbsreduce PPT aop z r
	   | RT.LPS PPT => lpsreduce PPT aop z r
	(* end case *))

    end (* local *)

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
			       concatWithoutBalancing (RT.parMap m 
					 (narySplitWithoutBalancing (unprd, RT.numAvailProcs ()))))
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
	   | RT.EBS_SP SST => ebsspmap SST f r
	   | RT.EBS_AP (K, V) => ebsapmap K V f r
	   | RT.LBS PPT => lbsmap PPT f r
	   | RT.LPS PPT => lpsmap PPT f r
	(* end case *))

    fun map' ss f r =
	(case ss
	  of RT.NO_SPLIT => nsmap f r
	   | RT.EBS_SP SST => ebsspmap SST f r
	   | RT.EBS_AP (K, V) => ebsapmap K V f r
	   | RT.LBS PPT => lbsmap PPT f r
	   | RT.LPS PPT => lpsmap PPT f r
	(* end case *))

    end (* local *)


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

	structure Z1 = Zipper1

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
				 appendWithoutBalancing (prd,
				   appendWithoutBalancing (RT.forkjoin (fn () => filt unprd1,
									fn () => filt unprd2)))
			     end
			   | COMPLETE r' => balanceIfNecessary r'
			(* end case *))
	    in
		filt r
	    end

	fun compose (f, g) = fn x => f (g x)

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
			       concat (RT.parMap (compose (balanceIfNecessary, filt)) 
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
	       | RT.EBS_SP SST => ebsspfilter SST pred r
	       | RT.EBS_AP (K, V) => ebsapfilter K 4 pred r
	       | RT.LBS PPT => lbsfilter PPT pred r
	       | RT.LPS PPT => lpsfilter PPT pred r
	    (* end case *))

	fun filter' ss pred r =
	    (case ss
	      of RT.NO_SPLIT => nsfilter pred r
	       | RT.EBS_SP SST => ebsspfilter SST pred r
	       | RT.EBS_AP (K, V) => ebsapfilter K 4 pred r
	       | RT.LBS PPT => lbsfilter PPT pred r
	       | RT.LPS PPT => lpsfilter PPT pred r
	    (* end case *))

    end (* local *)

    fun compose (f, g) = fn x => f (g x)

  (* partition f r *)
  (* applies f to each element x of r, from left to right, and returns a pair (pos, neg) where pos is *)
  (* the rope of those x for which f x evaluated to true, and neg is the rope of those for which f x *)
  (* evaluated to false. The elements of pos and neg retain the same relative order they possessed in r. *) 
    fun partition pred r =
	RT.forkjoin (fn () => filter pred r, fn () => filter (compose (not, pred)) r)

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
		    let val len = hi - lo + 1
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
				        appendWithoutBalancing (
				          RT.forkjoin (fn () => tab (lo', m - 1),
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
		    let val len = hi - lo + 1
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
	   | RT.EBS_SP SST => ebssptabulate SST (n, f)
	   | RT.EBS_AP (K, V) => ebsaptabulate K V (n, f)
	   | RT.LBS PPT => lbstabulate PPT (n, f)
	   | RT.LPS PPT => lpstabulate PPT (n, f)
	(* end case *))

    fun tabulate' ss (n, f) = 
	(case ss
	  of RT.NO_SPLIT => nstabulate f (0, n - 1)
	   | RT.EBS_SP SST => ebssptabulate SST (n, f)
	   | RT.EBS_AP (K, V) => ebsaptabulate K V (n, f)
	   | RT.LBS PPT => lbstabulate PPT (n, f)
	   | RT.LPS PPT => lpstabulate PPT (n, f)
	(* end case *))

    end

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

    (* all f r *)
    (* applies f to each element x of the rope r, from left to right, until f x evaluates to false; *)
    (* returns false if such an x exists and true otherwise. Equivalent to not(exists (not o f) r)). *)
  (* TODO: short-circuiting version of all *)
    fun all pred r = reduce (fn (x, y) => x andalso y) true (map pred r)

  end
