(* pmergesort-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Purely functional parallel mergesort.
 *)

functor PMergesortFn (
    structure K : ORD_KEY
    structure R : ROPES
  ) : sig

     structure K : ORD_KEY
     structure R : ROPES

  (* Sort a rope with ordered elements. For ropes of n elements, this operation has O(n*log^3 n) 
   * work and O(log^4 n) depth.
   *)
     val pMergeSort : K.ord_key R.rope -> K.ord_key R.rope

  end = struct

    structure K = K
    structure R = R

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
	  if R.length xs < R.length ys
	     then pMerge(ys, xs)
	  else if R.length xs = 0 orelse R.length ys = 0
	     then xs
	  else if R.length xs = 1 (* andalso R.length ys = 1 *)
	     then if K.compare(R.sub(xs, 0), R.sub(ys, 0)) = LESS
		     then R.concat(xs, ys)
		  else R.concat(ys, xs)
	  else let
             val (xsL, xsR) = R.splitAt(xs, R.length xs div 2)
	     val p = binarySearch(R.sub(xsR, 0), ys)
	     val (ysL, ysR) = R.splitAt(ys, p)
	     val l = pMerge(xsL, ysL)
	     val r = pMerge(xsR, ysR)
	     in
		R.concat(l, r)
	    end

    fun pMergeSort xs =
	  if R.length xs <= 1
	     then xs
	  else let
	     val (xsL, xsR) = R.splitAt(xs, R.length xs div 2)
	     val xsL = pMergeSort xsL
	     val xsR = pMergeSort xsR
	     in
		pMerge(xsL, xsR)
	     end

  end
