(* pmergesort.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Purely functional parallel mergesort.
 *)

structure PMergesort =
  struct

    structure R = Ropes

    fun lastElt arr = R.sub(arr, R.length arr - 1)

  (* return p such that xs[p] <= y <= xs[p+1] *)
  (* precondition: xs is sorted *)
    fun binarySearch cmp (y, xs) = 
	let
	    fun lp (a, b) =
	        if b = a then
		    a
		else
		    let
			val p = (b + a) div 2
			val (a, b) = 
			    if cmp (R.sub (xs, p), y) then
				(p + 1, b)
			    else 
				(a, p)
		    in
			lp (a, b)
		    end
        in
	    lp (0, R.length xs)
	end

  (* merge two sorted sequences *)
    fun pMerge cmp (xs, ys) =
	if R.length xs < R.length ys then
	    pMerge cmp (ys, xs)
	else if R.length xs = 0 orelse R.length ys = 0 then
	    xs
	else if R.length xs = 1 (* andalso R.length ys = 1 *) then
	    if cmp (R.sub (xs, 0), R.sub (ys, 0)) then
		R.concat (xs, ys)
	    else 
		R.concat (ys, xs)
	else 
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
		val (ysL, ysR) = R.cut (ys, binarySearch cmp (lastElt xsL, ys))
	    in
		R.concat (pMerge cmp (xsL, ysL), pMerge cmp (xsR, ysR))
	    end

    fun pMergesort cmp xs = 
	if R.length xs <= 1 then
	    xs
	else if R.length xs = 2 then
	    if cmp (R.sub (xs, 0), R.sub (xs, 1)) then
		xs
	    else
		R.revP xs
	else
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
	    in
		pMerge cmp (pMergesort cmp xsL, pMergesort cmp xsR)
	    end

  end
