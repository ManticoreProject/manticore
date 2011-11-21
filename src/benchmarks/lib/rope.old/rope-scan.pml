(* rope-scan-fn.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Follows the algorithms in Blelloch's "Prefix Sums and their Applications."
 * Adapted for ropes here.
 * (Nov 1990, CMS-CS-90-190)
 *
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure RopeScan =
  struct

    structure R = Rope

    structure RTy = RopeTy
    structure RT = Rope.RT

    val empty = R.empty
    val singleton = R.singleton
    val length = R.length
    val depth = R.depth
    val splitAt = R.splitAtWithoutBalancing
    val binarySplit = R.binarySplitWithoutBalancing
    val narySplit = R.narySplitWithoutBalancing
    val append = R.appendWithoutBalancing
    val concat = R.concatWithoutBalancing
    val reduce = R.reduce
    val sub = R.sub
    val balanceIfNecessary = R.balanceIfNecessary

    structure S = RTy.S

    datatype rope = datatype RTy.rope
    datatype either = datatype Either.either
    datatype progress = datatype ProgressTy.progress

  (* same as a rope, but with a cached value at each cat node *)
    datatype 'a crope
      = CLeaf of 'a * 'a S.seq
      | CCat of 'a * int * int * 'a crope * 'a crope

    fun datum crp =
	(case crp
	  of CLeaf (d, _) => d
	   | CCat (d, _, _, _, _) => d
	(* end case *))

    fun setDatum (crp, dtm) =
	(case crp
	  of CLeaf (_, s) => CLeaf (dtm, s)
	   | CCat (_, d, len, cl, cr) => CCat (dtm, d, len, cl, cr)
	(* end case *))

    fun cdepth crp =
	(case crp
	  of CLeaf (_, s) => 0
	   | CCat (_, d, _, _, _) => d
	(* end case *))

    fun clength crp =
	(case crp
	  of CLeaf (_, s) => S.length s
	   | CCat (_, _, len, _, _) => len
	(* end case *))

    fun cappend (aop, cl, cr) = 
	if clength cl = 0 then cr
	else if clength cr = 0 then cl
	else
	    let 
		val d = Int.max (cdepth cl, cdepth cr)
		val len = clength cl + clength cr
	    in
		CCat (aop (datum cl, datum cr), d, len, cl, cr)
	    end

    fun cconcat (z, aop, crps) =
	List.foldl (fn (cl, cr) => cappend (aop, cl, cr)) (CLeaf (z, S.empty ())) (List.rev crps)

    fun cleaf (z, aop, s) = CLeaf (S.foldl aop z s, s)

    fun cempty z = CLeaf (z, S.empty ())

    fun csingleton x = CLeaf (x, S.singleton x)

  (* creates a new list from a rope of elements *)
    fun toList r = let
      fun build r =
	  (case r
	    of CLeaf (_, s) => S.toList s
	     | CCat (_, _, _, rL, rR) => build rL @ build rR
	  (* end case *))
      in
        build r
      end

    fun cSplitAt (z, aop, r, i) =
     (case r
        of CLeaf (_, s) => let
	     val (s1, s2) = S.cut (s, i+1)
	     in
	       (cleaf (z, aop, s1), cleaf (z, aop, s2))
	     end
	 | CCat (_, depth, len, r1, r2) =>
	     if i = clength r1 - 1 then
               (r1, r2)
	     else if i < clength r1 then let
               val (r11, r12) = cSplitAt (z, aop, r1, i)
               in
                 (r11, cappend (aop, r12, r2))
               end
	     else let
               val (r21, r22) = cSplitAt (z, aop, r2, i - clength r1)
               in
                 (cappend (aop, r1, r21), r22)
               end
        (* end case *))

    fun cbinarySplit (z, aop, cr) = cSplitAt (z, aop, cr, clength cr div 2 - 1)

  (* cnarySplit (z, aop, r, k) *)
  (* split r into MIN(length r, k) subropes; each subrope has roughly the same length *)
  (* pre: k > 0 *)
    fun cnarySplit (z, aop, r, k) = 
	let val minc = if clength r <= k then 1 else clength r div k  (* min subrope size *)
	    val maxc = Int.max (clength r div k + clength r mod k, minc) (* max subrope size *)
	    fun lp (r, rps) =
		if clength r <= maxc then List.rev (r :: rps)
		else let val (r1, r2) = cSplitAt (z, aop, r, minc - 1)
		     in
			 lp (r2, r1 :: rps)
		     end
	    fun mkLeaf x = cleaf (z, aop, S.singleton x)
	in
	    if clength r <= k then List.map mkLeaf (toList r)
	    else if k > 0 then lp (r, nil)
	    else raise Fail "cnarySplit: k < 1"
	end

  (* pre: inBounds (r, i) *)
    fun csub (r, i) =
	(case r
	  of CLeaf (_, s) => S.sub (s, i)
	   | CCat (_, _, len, r1, r2) =>
             if i < clength r1 then csub (r1, i)
	     else csub (r2, i - clength r1)
	(* end case *))

    datatype 'a ctx
      = Top
      | L of 'a ctx * ('a rope, 'a crope) either
      | R of ('a rope, 'a crope) either * 'a ctx

    type 'a loc = ('a rope, 'a crope) either * 'a ctx

    fun up (aop, loc) =
	(case loc
	  of (_, Top) => NONE
	   | (LEFT rp, L (c, LEFT r)) => SOME (LEFT (append (rp, r)), c)
	   | (RIGHT crp, L (c, RIGHT cr)) => SOME (RIGHT (cappend (aop, crp, cr)), c)
	   | (LEFT rp, R (LEFT l, c)) => SOME (LEFT (append (l, rp)), c)
	   | (RIGHT crp, R (RIGHT cl, c)) => SOME (RIGHT (cappend (aop, cl, crp)), c)
	   | _ => raise Fail "error"
	(* end case *))

    fun upmost (aop, loc) =
	(case up (aop, loc)
	  of NONE => loc
	   | SOME loc' => upmost (aop, loc'))

    fun left loc =
	(case loc
	  of (LEFT (Leaf _), _) => NONE
	   | (RIGHT (CLeaf _), _) => NONE
	   | (LEFT (Cat (_, _, l, r)), c) => SOME (LEFT l, L (c, LEFT r))
	   | (RIGHT (CCat (_, _, _, cl, cr)), c) => SOME (RIGHT cl, L (c, RIGHT cr)))

    fun leftmost loc =
	(case left loc
	  of NONE => loc
	   | SOME loc' => leftmost loc')

    fun upUntilLThenRight (aop, loc) = 
	(case loc
	  of (rp, L (c, r)) => SOME (r, R (rp, c))
	   | _ => (case up (aop, loc)
		    of NONE => NONE
		     | SOME loc' => upUntilLThenRight (aop, loc')))

    fun input crp = (RIGHT crp, Top)
		    
    fun output (aop, loc) = 
	(case upmost (aop, loc)
	  of (LEFT rp, _) => rp
	   | _ => raise Fail "error")

    fun cOutput (aop, loc) =
	(case upmost (aop, loc)
	  of (RIGHT crp, _) => crp
	   | _ => raise Fail "error")

    fun cInput rp = (LEFT rp, Top)

    fun datumOfLoc loc =
	(case loc
	  of (RIGHT crp, _) => datum crp
	   | _ => raise Fail "error")

    fun revCtx c =
	let fun rev c =
		(case c
		  of (Top, c') => c'
		   | (L (c, r), c') => rev (c, L (c', r))
		   | (R (l, c), c') => rev (c, R (l, c'))
		(* end case *))
	in
	    rev (c, Top)
	end

    fun splitUpsweep (z, aop, unprds, prd, c) =
	let
	    fun lp (c, unprdr, prdr) =
		(case c
		  of Top => (unprdr, cappend (aop, prdr, cleaf (z, aop, prd)))
		   | L (c, LEFT r) => lp (c, append (r, unprdr), prdr)
		   | R (RIGHT l, c) => lp (c, unprdr, cappend (aop, prdr, l))
		   | _ => raise Fail "error"
		(* end case *))
	in
	    lp (revCtx (L (c, LEFT (Leaf unprds))), empty (), CLeaf (z, S.fromList nil))
	end

    fun splitDownsweep (z, aop, (v, unprds), prds, c) =
	let
	    fun lp (c, unprdr, prdr) =
		(case c
		  of Top => ((unprdr, v), append (prdr, Leaf prds))
		   | L (c, RIGHT r) => lp (c, cappend (aop, r, unprdr), prdr)
		   | R (LEFT l, c) => lp (c, unprdr, append (prdr, l))
		   | _ => raise Fail "error"
		(* end case *))
	in
	    lp (revCtx (L (c, RIGHT (cleaf (z, aop, unprds)))), CLeaf (z, S.fromList nil), empty ())
	end

    local

	fun nsscan aop z rp =
	    let
		fun scan (rp, acc) =
		    (case rp
		      of Leaf s => 
			 let
			     val (s, acc) = S.scanl aop acc s
			 in
			     (Leaf s, acc)
			 end
		       | Cat (d, len, l, r) =>
			 let
			     val (l', acc) = scan (l, acc)
			     val (r', acc) = scan (r, acc)
			 in
			     (Cat (d, len, l', r'), acc)
			 end
		    (* end case *))
		val (rp', _) = scan (rp, z)
	    in
		rp'
	    end

	fun nsupsweep aop z rp = 
	    let fun upswp rp =
		    (case rp
		      of Leaf s => cleaf (z, aop, s)
		       | Cat (d, len, l, r) =>
			 let val cl = upswp l
			     val cr = upswp r
			 in
			     CCat (aop (datum cl, datum cr), d, len, cl, cr)
			 end
		    (* end case *))
	    in
		upswp rp
	    end

	fun nsdownsweep aop z crp = 
	    let fun downswp (acc, crp) =
		    (case crp
		      of CLeaf (_, s) => 
			 let 
			     val (s, _) = S.scanl aop acc s
			 in
			     Leaf s
			 end
		       | CCat (_, d, len, cl, cr) =>
			 Cat (d, len, downswp (acc, cl), downswp (aop (acc, datum cl), cr))
		    (* end case *))
	    in
		downswp (z, crp)
	    end

	fun ebsspupsweep SST aop z rp =
	    let fun upswp rp =
		    if length rp <= SST then
			nsupsweep aop z rp
		    else
			let val (l, r) = binarySplit rp
			    val (cl, cr) = RT.forkjoin (fn () => upswp l, fn () => upswp r)
			in
			    CCat (aop (datum cl, datum cr), depth rp, length rp, cl, cr)
			end
	    in
		upswp rp
	    end

	fun ebsspdownsweep SST aop z crp =
	    let fun downswp (crp, acc) =
		    if clength crp <= SST then
			nsdownsweep aop acc crp
		    else
			let 
			    val (cl, cr) = cbinarySplit (z, aop, crp)
			    val (rl, rr) = RT.forkjoin (fn () => downswp (cl, acc), 
							fn () => downswp (cr, aop (datum cl, acc)))
			in
			    Cat (cdepth crp, clength crp, rl, rr)
			end
	    in
		downswp (crp, z)
	    end

	fun listPrefix aop z xs =
	    let
		fun pfx (xs, acc, res) =
		    (case xs
		      of nil => List.rev res
		       | x :: xs => pfx (xs, aop (x, acc), acc :: res))
	    in
		pfx (xs, z, nil)
	    end

	fun ebsapupsweep K V aop z rp =
	    let
		fun m (rp, n, wid) =
		    if length rp = 0 orelse length rp = 1 orelse n <= 1 then 
			nsupsweep aop z rp
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				m (rp, Int.max (n, V), curWid)
			    else
				let
				    val (rp1, rp2) = binarySplit rp
				    val n' = n div 2
				    val (cl, cr) = RT.forkjoin (fn () => m (rp1, n', curWid),
								fn () => m (rp2, n', curWid))
				in
				    CCat (aop (datum cl, datum cr), depth rp, length rp, cl, cr)
				end
			end
	    in
		m (rp, K * RT.numAvailProcs (), RT.workerId ())
	    end

	fun ebsapdownsweep K V aop z crp =
	    let
		fun m (crp, acc, n, wid) =
		    if clength crp = 0 orelse clength crp = 1 orelse n <= 1 then 
			nsdownsweep aop acc crp
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				m (crp, acc, Int.max (n, V), curWid)
			    else
				let
				    val (crp1, crp2) = cbinarySplit (z, aop, crp)
				    val n' = n div 2
				    val acc' = aop (datum crp1, acc)
				    val (rl, rr) = RT.forkjoin (fn () => m (crp1, acc, n', curWid),
								fn () => m (crp2, acc', n', curWid))
				in
				    Cat (cdepth crp, clength crp, rl, rr)
				end
			end
	    in
		m (crp, z, K * RT.numAvailProcs (), RT.workerId ())
	    end

	fun upsweepUntil k cond aop z rp =
	    let
		fun upswp loc =
		    let
			val (LEFT (Leaf s), c) = leftmost loc
		    in
			case S.reduceUntil k cond aop z s
			 of COMPLETE v =>
			    (case upUntilLThenRight (aop, (RIGHT (CLeaf (v, s)), c))
			      of NONE => COMPLETE (cOutput (aop, (RIGHT (CLeaf (v, s)), c)))
			       | SOME loc' => upswp loc')
			  | PARTIAL ((unprd, prd), v) => PARTIAL (splitUpsweep (z, aop, unprd, prd, c))
		    end
	    in
		upswp (cInput rp)
	    end

	fun downsweepUntil k cond aop z acc crp =
	    let
		fun downswp (loc, acc) =
		    let
			val (RIGHT (CLeaf (_, s)), c) = leftmost loc
		    in
			case S.scanlUntil k cond aop acc s
			 of COMPLETE x =>
			    let val (s', acc') = x
			    in
				(case upUntilLThenRight (aop, (LEFT (Leaf s'), c))
				  of NONE => COMPLETE (output (aop, (LEFT (Leaf s'), c)))
				   | SOME loc' => downswp (loc', acc')
				(* end case *))
			    end
			  | PARTIAL (unprd, (prd, acc)) =>
			    PARTIAL (splitDownsweep (z, aop, (acc, unprd), prd, c))
		    end
	    in
		downswp (input crp, acc)
	    end

	fun lbsupsweep PPT aop z rp =
	    let
		fun upswp rp =
		    if length rp = 0 then cempty z
		    else if length rp = 1 then csingleton (sub (rp, 0))
		    else
			(case upsweepUntil PPT RT.otherHungryProcs aop z rp
			  of PARTIAL (unprd, prd) =>
			     let
				 val (unprd1, unprd2) = binarySplit unprd
				 val (prd1, prd2) = RT.forkjoin (fn () => upswp unprd1, fn () => upswp unprd2)
			     in
				 cappend (aop, prd, cappend (aop, prd1, prd2))
			     end
			   | COMPLETE crp => crp
		    (* end case *))
	    in
		upswp rp
	    end

	fun lbsdownsweep PPT aop z crp =
	    let
		fun downswp (crp, acc) =
		    if clength crp = 0 then empty ()
		    else if clength crp = 1 then singleton acc
		    else
			(case downsweepUntil PPT RT.otherHungryProcs aop z acc crp
			  of PARTIAL ((unprd, acc'), prd) =>
			     let
				 val (unprd1, unprd2) = cbinarySplit (z, aop, unprd)
				 val (prd1, prd2) = 
				     RT.forkjoin (fn () => downswp (unprd1, acc'), 
						  fn () => downswp (unprd2, aop (acc', datum unprd1)))
			     in
				 append (prd, append (prd1, prd2))
			     end
			   | COMPLETE rp => balanceIfNecessary rp
			(* end case *))
	    in
		downswp (crp, z)
	    end

	fun lpsupsweep PPT aop z rp =
	    let
		fun upswp rp =
		    if length rp = 0 then cempty z
		    else if length rp = 1 then csingleton (sub (rp, 0))
		    else
			(case upsweepUntil PPT RT.otherHungryProcs aop z rp
			  of PARTIAL (unprd, prd) =>
			     let
				 val prds = RT.parMap upswp (narySplit (unprd, RT.numAvailProcs ()))
			     in
				 cappend (aop, prd, cconcat (z, aop, prds))
			     end
			   | COMPLETE crp => crp
			(* end case *))
	    in
		upswp rp
	    end

	fun lpsdownsweep PPT aop z crp =
	    let
		fun downswp (crp, acc) =
		    if clength crp = 0 then empty ()
		    else if clength crp = 1 then singleton acc
		    else
			(case downsweepUntil PPT RT.otherHungryProcs aop z acc crp
			  of PARTIAL ((unprd, acc'), prd) =>
			     let
				 val unprds = cnarySplit (z, aop, unprd, RT.numAvailProcs ())
				 val chunks = ListPair.zip (unprds, listPrefix aop acc' (List.map datum unprds))
			     in
				 append (prd, concat (RT.parMap downswp chunks))
			     end
			   | COMPLETE rp => balanceIfNecessary rp
			(* end case *))
	    in
		downswp (crp, z)
	    end
	
    in

    fun scanl aop z rp = 
	(case RT.splittingStrategy
	  of RT.NO_SPLIT => nsscan aop z rp
	   | RT.EBS_SP SST => ebsspdownsweep SST aop z (ebsspupsweep SST aop z rp)
	   | RT.EBS_AP (K, V) => ebsapdownsweep K V aop z (ebsapupsweep K V aop z rp)
	   | RT.LBS PPT => lbsdownsweep PPT aop z (lbsupsweep PPT aop z rp)
	   | RT.LPS PPT => lpsdownsweep PPT aop z (lpsupsweep PPT aop z rp)
	(* end case *))

    end

  end
