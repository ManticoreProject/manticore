(* pmergesort-with-seq-bc-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Purely functional parallel mergesort. This algorithm takes advantage of the rope
 * data structure to choose a suitable sequential cutoff.
 *)

functor PMergesortWithSeqBcFn (

    structure K : ORD_KEY
    structure R : ROPE

  (* sequential merge *)
    val sMerge  : K.ord_key R.seq * K.ord_key R.seq -> K.ord_key R.seq
  (* sequential sort *)
    val sSort   : K.ord_key R.seq -> K.ord_key R.seq

  ) : sig

     structure K : ORD_KEY
     structure R : ROPE

  (* Sort a rope with ordered elements. For ropes of n elements, this operation has O(n*log^3 n) 
   * work and O(log^4 n) depth.
   *)
     val pMergesort : K.ord_key R.rope -> K.ord_key R.rope

  end = struct

    structure K = K
    structure R = R

    fun lessThan (x, y) = (
	  case K.compare(x, y)
	   of LESS => true
	    | _ => false
          (* end case *))

  (* split xs into xs[0, ..., n-1] and xs[n, ..., |xs|] *)
    fun split (xs, n) =
	  if n = 0
	     then (R.empty, xs)
	  else R.splitAt(xs, n - 1)

  (* assuming that xs is sorted, return p such that xs[p] <= y <= xs[p+1]. this
   * operation performs O(log^2 n) comparisons.
   *)
    fun binarySearch (y, xs) = let
	  fun lp (a, b) =
	        if b = a
		   then a
		else let
	           val p = (b + a) div 2
		   val (a, b) = if K.compare(R.sub(xs, p), y) = LESS
				   then (p + 1, b)
				else (a, p)
		   in
		     lp(a, b)
		   end
          in
	    lp(0, R.length xs)
	  end

  (* merge two sorted collections *)
    fun pMerge (xs, ys) =
	  if R.isLeaf xs andalso R.isLeaf ys
	     then (* no parallelism to find here *)
	      R.fromSeq(sMerge(R.toSeq xs, R.toSeq ys))
	  else if R.length xs < R.length ys
	     then pMerge(ys, xs)
	  else if R.length xs = 0 orelse R.length ys = 0
	     then xs
	  else if R.length xs = 1 (* andalso R.length ys = 1 *)
	     then if K.compare(R.sub(xs, 0), R.sub(ys, 0)) = LESS
		     then R.concat(xs, ys)
		  else R.concat(ys, xs)
	  else let
	     val (xsL, xsR) = split(xs, R.length xs div 2)
	     val p = binarySearch(R.sub(xsR, 0), ys)
	     val (ysL, ysR) = split(ys, p)
	     val l = pMerge(xsL, ysL)
	     val r = pMerge(xsR, ysR)
	     in
		R.concat(l, r)
	     end

    fun pMergesort xs =
	  if R.isLeaf xs
	     then
	      (* no parallelism to find here *)
	      R.fromSeq(sSort(R.toSeq xs))
	  else if R.length xs <= 1
	     then xs
	  else let
	     val (xsL, xsR) = split(xs, R.length xs div 2)
	     val xsL = pMergesort xsL
	     val xsR = pMergesort xsR
	     in
		pMerge(xsL, xsR)
	     end

  end
