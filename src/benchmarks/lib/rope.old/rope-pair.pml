(* rope-pair.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

structure RopePair =
  struct

    structure RT = Rope.RT
    structure RTy = Rope.RTy
    structure SPair = VectorSeqPair

    val append = RTy.appendWithoutBalancing
    val concat = Rope.concatWithoutBalancing
    val empty = RTy.empty
    val singleton = RTy.singleton
    val length = RTy.length
    val binarySplit = Rope.binarySplitWithoutBalancing
    val narySplit = Rope.narySplitWithoutBalancing
    val sub = RTy.sub
    val balanceIfNecessary = Rope.balanceIfNecessary

    datatype rope = datatype RTy.rope
    datatype either = datatype Either.either
    datatype progress2 = datatype ProgressTy.progress2

    structure Z1 = Zipper1
    structure Z2 = Zipper2

    fun unequalLengths () = RTy.failwith "RopePair: unequal lengths"

    local
    
	fun nsMapEq f (rp1, rp2) =
	    let
		fun dfs (loc1, loc2) =
		    let val (LEFT (Leaf s1), c1) = Z2.leftmost loc1
			val (Leaf s2, c2) = Z1.leftmost loc2
		    in
			case (SPair.map f (s1, s2), SPair.leftover (s1, s2))
			 of (s1prd, NONE) =>
			    (* |s1| = |s2| *)
			    (case (Z2.upUntilLThenRight (RIGHT (Leaf s1prd), c1),
				   Z1.upUntilLThenRight (empty (), c2))
			      of (NONE, NONE) => Z2.output (RIGHT (Leaf s1prd), c1)
			       | (SOME loc1', SOME loc2') => dfs (loc1', loc2')
			       | _ => unequalLengths ()
			    (* end case *))
			  | (s1prd, SOME (RIGHT s2unprd)) =>
			    (* |s1| < |s2| *)
			    (case Z2.upUntilLThenRight (RIGHT (Leaf s1prd), c1)
			      of NONE => Z2.output (RIGHT (Leaf s1prd), c1)
			       | SOME loc1' => dfs (loc1', (Leaf s2unprd, c2))
			    (* end case *))
			  | (s1prd, SOME (LEFT s1unprd)) =>
			    (* |s1| > |s2| *)
			    let val loc1' = (LEFT (Leaf s1unprd), Z2.R (RIGHT (Leaf s1prd), c1))
			    in
				case Z1.upUntilLThenRight (empty (), c2)
				 of NONE => unequalLengths ()
				  | SOME loc2' => dfs (loc1', loc2')
			    end
		    end
	    in
		dfs (Z2.input rp1, Z1.input rp2)
	    end

	fun ebsspmapEq SST f (rp1, rp2) =
	    let
		fun mp (rp1, rp2) =
		    if length rp1 <= SST then nsMapEq f (rp1, rp2)
		    else
			let
			    val (rp11, rp12) = binarySplit rp1
			    val (rp21, rp22) = binarySplit rp2
			in
			    append (RT.forkjoin (fn () => mp (rp11, rp21),
						 fn () => mp (rp12, rp22)))
			end
	    in
		mp (rp1, rp2)
	    end

	fun ebsapmapEq K V f (rp1, rp2) =
	    let
		fun m (rp1, rp2, n, wid) =
		    if length rp1 = 0 then empty ()
		    else if length rp1 = 1 then singleton (f (sub (rp1, 0), sub (rp2, 0)))
		    else if n <= 1 then nsMapEq f (rp1, rp2)
		    else 
			let
			    val curWid = RT.workerId ()
			in
			    if wid <> curWid then
				(* this thread was stolen *)
				m (rp1, rp2, Int.max (n, V), curWid)
			    else
				let
				    val (rp11, rp12) = binarySplit rp1
				    val (rp21, rp22) = binarySplit rp2
				    val n' = n div 2
				in
				    append (RT.forkjoin (fn () => m (rp11, rp21, n', curWid),
							 fn () => m (rp12, rp22, n', curWid)))
				end
			end
	    in
		m (rp1, rp2, K * RT.numAvailProcs (), RT.workerId ())
	    end

	fun mapEqUntil k cond f (rp1, rp2) =
	    let
		fun dfs (loc1, loc2) =
		    let 
			val (LEFT (Leaf s1), c1) = Z2.leftmost loc1
			val (Leaf s2, c2) = Z1.leftmost loc2
		    in
			case SPair.mapUntil k cond f (s1, s2)
			 of PARTIAL2 (s1unprd, s2unprd, s1prd) =>
			    let val (rp1, prd) = Z2.split (s1unprd, s1prd, c1)
				val rp2 = Z1.output (Leaf s2unprd, c2)
			    in
				PARTIAL2 (rp1, rp2, prd)
			    end
			  | COMPLETE2 s1prd =>
			    (case SPair.leftover (s1, s2)
			      of NONE =>
				 (* |s1| = |s2| *)
				 (case (Z2.upUntilLThenRight (RIGHT (Leaf s1prd), c1),
					Z1.upUntilLThenRight (empty (), c2))
				   of (NONE, NONE) => COMPLETE2 (Z2.output (RIGHT (Leaf s1prd), c1))
				    | (SOME loc1', SOME loc2') => dfs (loc1', loc2')
				    | _ => unequalLengths ()
				 (* end case *))
			       | SOME (RIGHT s2unprd) =>
				 (* |s1| < |s2| *)
				 (case Z2.upUntilLThenRight (RIGHT (Leaf s1prd), c1)
				   of NONE => COMPLETE2 (Z2.output (RIGHT (Leaf s1prd), c1))
				    | SOME loc1' => dfs (loc1', (Leaf s2unprd, c2))
				 (* end case *))
			       | SOME (LEFT s1unprd) =>
				 (* |s1| > |s2| *)
				 let val loc1' = (LEFT (Leaf s1unprd), Z2.R (RIGHT (Leaf s1prd), c1))
				 in
				     case Z1.upUntilLThenRight (empty (), c2)
				      of NONE => unequalLengths ()
				       | SOME loc2' => dfs (loc1', loc2')
				 end
			    (* end case *))
		    end
	    in
		dfs (Z2.input rp1, Z1.input rp2)
	    end

	fun lbsMapEq PPT f (rp1, rp2) =
	    let 
		fun m (rp1, rp2) =
		    if length rp1 = 0 then empty ()
		    else if length rp1 = 1 then singleton (f (sub (rp1, 0), sub (rp2, 0)))
		    else
			(case mapEqUntil PPT RT.otherHungryProcs f (rp1, rp2)
			  of PARTIAL2 (unprd1, unprd2, prd) =>
			     let 
				 val (unprd1l, unprd1r) = binarySplit unprd1
				 val (unprd2l, unprd2r) = binarySplit unprd2
			     in
				 append (prd, 
				   append (RT.forkjoin (fn () => m (unprd1l, unprd2l),
							fn () => m (unprd1r, unprd2r))))
			     end
			   | COMPLETE2 rp => balanceIfNecessary rp
			(* end case *))
	    in
		m (rp1, rp2)
	    end

	fun lpsMapEq PPT f (rp1, rp2) =
	    let fun m (rp1, rp2) =
		    if length rp1 = 0 then empty ()
		    else if length rp1 = 1 then singleton (f (sub (rp1, 0), sub (rp2, 0)))
		    else
			(case mapEqUntil PPT RT.otherHungryProcs f (rp1, rp2)
			  of PARTIAL2 (unprd1, unprd2, prd) =>
			     let
				 val unprdPairs = ListPair.zip (narySplit (unprd1, RT.numAvailProcs ()),
								narySplit (unprd2, RT.numAvailProcs ()))
			     in
				 append (prd, concat (RT.parMap m unprdPairs))
			     end
			   | COMPLETE2 rp => balanceIfNecessary rp
			(* end case *))
	    in
		m (rp1, rp2)
	    end

    in

    fun mapEq f (rp1, rp2) = 
	if length rp1 <> length rp2 then unequalLengths ()
	else (case RT.splittingStrategy
	       of RT.NO_SPLIT => nsMapEq f (rp1, rp2)
		| RT.EBS_SP SST => ebsspmapEq SST f (rp1, rp2)
		| RT.EBS_AP (K, V) => ebsapmapEq K V f (rp1, rp2)
		| RT.LBS PPT => lbsMapEq PPT f (rp1, rp2)
		| RT.LPS PPT => lpsMapEq PPT f (rp1, rp2)
	     (* end case *))

    end

    fun zipEq (rp1, rp2) = mapEq (fn x => x) (rp1, rp2)

  end
