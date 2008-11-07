(* pmergesort.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Purely functional parallel mergesort.
 *)

functor PMergesort (
    
    type item
    type collection

  (* O(C)-time operations *)
    val compare : (item * item) -> order
    val empty : collection
    val length : collection -> int

  (* O(log n)-time operations *)
  (* subscript *)
    val sub : (collection * int) -> item
  (* given an index i and a collection c, split c into collections (c[0, ..., i-1], c[i, ..., n]) *)
    val splitAt : (int * collection) -> (collection * collection)
  (* concatenate *)
    val concat : (collection * collection) -> collection

  ) = struct

  (* assuming that xs is sorted, returns p such that xs[p] <= y <= xs[p+1]. this
   * operation performs O(log^2 n) comparisons.
   *)
    fun binarySearch (y, xs) = let
	  fun lp (a, b) =
	        if b = a
		   then a
		else let
	           val p = (b + a) div 2
		   val (a, b) = if compare(sub(xs, p), y) = LESS
				   then (p + 1, b)
				else (a, p)
		   in
		     lp(a, b)
		   end
          in
	    lp(0, length xs)
	  end

  (* merge two sorted collections *)
    fun merge (xs, ys) =
	  if length xs < length ys
	     then merge(ys, xs)
	  else if length xs = 0 orelse length ys = 0
	     then xs
	  else if length xs = 1 (* andalso length ys = 1 *)
	     then if compare(sub(xs, 0), sub(ys, 0)) = LESS
		     then concat(xs, ys)
		  else concat(ys, xs)
	  else let
             val (xsL, xsR) = splitAt(length xs div 2, xs)
	     val p = binarySearch(sub(xsR, 0), ys)
	     val (ysL, ysR) = splitAt(p, ys)
	     val l = merge(xsL, ysL)
	     val r = merge(xsR, ysR)
	     in
		concat(l, r)
	    end

    fun mergeSort xs =
	  if length xs = 0
	     then empty
	  else let
	     val (xsL, xsR) = splitAt(length xs div 2, xs)
	     val xsL = mergeSort xsL
	     val xsR = mergeSort xsR
	     in
		merge(xsL, xsR)
	     end

  end
