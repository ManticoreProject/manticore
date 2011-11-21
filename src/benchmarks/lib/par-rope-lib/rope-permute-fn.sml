(* rope-permute-fn.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations to permute the elements of a rope
 *
 * Authors:
 *   Mike Rainey (mrainey@cs.uchicago.edu)
 *   Adam Shaw (ams@cs.uchicago.edu)
 *
 *)

functor RopePermuteFn (

    structure RTy : ROPE_TY
    structure RT : RUNTIME

    val maxLeafSize : int
    val sub : 'a RTy.rope * int -> 'a
    val length : 'a RTy.rope -> int
    val splitAt    : 'a RTy.rope * int -> ('a RTy.rope * 'a RTy.rope)
    val partition  : ('a -> bool) -> 'a RTy.rope -> ('a RTy.rope * 'a RTy.rope)
    val append : 'a RTy.rope * 'a RTy.rope -> 'a RTy.rope
    val map        : ('a -> 'b) -> 'a RTy.rope -> 'b RTy.rope
    val tabulate   : (int * (int -> 'a)) -> 'a RTy.rope
    val app        : ('a -> unit) -> 'a RTy.rope -> unit
    val delete     : 'a RTy.rope * int -> 'a RTy.rope
    val singleton  : 'a -> 'a RTy.rope

  ) : ROPE_PERMUTE where type 'a rope = 'a RTy.rope = 
  struct

    type 'a rope = 'a RTy.rope

    fun seqScatter (lo, values, ivpairs) =
	if length values = 0 then values 
	else
	    let
		val len = length values
		val a = Array.tabulate (len, fn i => sub (values, i))
	    in
		app (fn (ix, v) => Array.update (a, ix - lo, v)) ivpairs;
		tabulate (len, fn k => Array.sub (a, k))
	    end

    fun scatter (values, ivpairs) =
	let
	    (* divide & conquer scatter operation *)
	    (* the divide step: divide values in half, values[0, ..., m-1] and values[m, ..., |values|]. *)
	    (* then partition ivpairs by the indices that go in each half of values,  *)
	    (* { (i, v) s.t. i < m, (i, v) in ivpairs} and { (i, v) s.t. i >= m, (i, v) in ivpairs} *)
	    (* then recur on each half and append the two result value lists. *)
	    (* do a sequential scatter operation at a leaf. *)
	    (* TODO: implement in the rope library and use LBS, EBS, etc. *)
	    (* NOTE: a more robust algorithm would remove duplicate indices from ivpairs *)
	    fun scttr (lo, values, ivpairs) =
		if length values < maxLeafSize orelse length values <= 2 
		then seqScatter (lo, values, ivpairs)
		else
		    let
			val m = length values div 2
			val mIx = m + lo
			val (valuesL, valuesR) = splitAt (values, m)
			val (ivpairsL, ivpairsR) = partition (fn (idx, v) => idx <= mIx) ivpairs
			val (valuesL', valuesR') = RT.forkjoin (fn () => scttr (lo, valuesL, ivpairsL), 
								fn () => scttr (mIx + 1, valuesR, ivpairsR))
		    in
			append (valuesL', valuesR')
		    end
	in
	    scttr (0, values, ivpairs)
	end

    fun gather (values, indices) = map (fn i => sub (values, i)) indices

    (* rotate the sequence by one position to the right *)
    fun rotateRight rp =
	if length rp = 0 then rp 
	else
	    let
		val endIx = length rp - 1
		val rEnd = sub (rp, endIx)
		val rp' = delete (rp, endIx)
	    in
		append (singleton rEnd, rp')
	    end

    (* given a sequence and an integer, rotate the sequence around by n positions to the right *)
    (* if the integer is negative, the sequence is rotated to the left. *)
    (* e.g., rotate ([1,2,3,4,5,6,7], 3) ==> [5,6,7,1,2,3,4] *)
    fun rotate (rp, n) =
	let
	    fun right (rp, i) = if i < n then right (rotateRight rp, i + 1) else rp
	in
	    if n = 0 then rp
	    else if n > 0 then right (rp, 0)
	    else raise Fail "todo"
	end
				 
  end
