(* pmergesort.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Purely functional parallel mergesort.
 *)

structure PMergesort =
  struct

    structure K = Int
    structure R = Ropes

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
		   val (a, b) = (
		         case K.compare(R.sub(xs, p), y) 
			  of LESS => (p + 1, b)
			   | _ => (a, p)
 		         (* end case *))
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
	     then if lessThan(R.sub(xs, 0), R.sub(ys, 0))
		     then R.concat(xs, ys)
		  else R.concat(ys, xs)
	  else let
             val (xsL, xsR) = split(xs, R.length xs div 2)
	     val p = binarySearch(R.sub(xsR, 0), ys)
	     val (ysL, ysR) = split(ys, p)
	     pval l = pMerge(xsL, ysL)
	     val r = pMerge(xsR, ysR)
	     in
		R.concat(l, r)
	    end

    fun pMergeSort xs =
	  if R.length xs <= 1
	     then xs
	  else let
	     val (xsL, xsR) = split(xs, R.length xs div 2)
	     pval xsL = pMergeSort xsL
	     val xsR = pMergeSort xsR
	     in
		pMerge(xsL, xsR)
	     end

  end
