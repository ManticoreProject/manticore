(* pmergesort-with-seq-bc.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Hybrid parallel mergesort. When the input drops below a threshold, the algorithm
 * uses a sequential, in-place counterpart.
 *
 *)

structure PMergesortWithSeqBc =
  struct

    structure R = Rope

    fun less ord = 
	(case ord
	  of LESS => true
	   | _ => false)

    fun copy s = R.tabP (R.length s, fn i => R.sub (s, i))

    fun lastElt arr = R.sub(arr, R.length arr - 1)

  (* in-place, sequential merge of two sorted sequences *)
  (* precondition: xs is non empty *)
  (* precondition: xs has at least as many elements as ys *)  
    fun sMerge cmp (xs, ys) =
	let
	    val n = R.S.length xs + R.S.length ys
	    val zs = R.S.tabulate (n, fn i => R.S.sub (xs, 0))
	    fun merge (i, xsI, ysI) =
		if i < n then
		    if ysI > R.S.length ys - 1 then
			(R.S.update (zs, i, R.S.sub (xs, xsI));
			 merge (i + 1, xsI + 1, ysI))
		    else if xsI > R.S.length xs - 1 then
			(R.S.update (zs, i, R.S.sub (ys, ysI));
			 merge (i + 1, xsI, ysI + 1))
		    else if less (cmp (R.S.sub (xs, xsI), R.S.sub (ys, ysI))) then
			(R.S.update (zs, i, R.S.sub (xs, xsI));
			 merge (i + 1, xsI + 1, ysI))
		    else
			(R.S.update (zs, i, R.S.sub (ys, ysI));
			 merge (i + 1, xsI, ysI + 1))
		else
		    ()
	in
	    merge (0, 0, 0);
	    zs
	end

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
			    if less (cmp (R.sub (xs, p), y)) then
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
	else if R.isLeaf xs then
	    let
		val (R.LEAF (l1, xsS), R.LEAF (l2, ysS)) = (xs, ys)
	    in
		R.LEAF (l1 + l2, sMerge cmp (xsS, ysS))
	    end
	else if R.length xs = 0 orelse R.length ys = 0 then
	    xs
	else if R.length xs = 1 (* andalso R.length ys = 1 *) then
	    if less (cmp (R.sub (xs, 0), R.sub (ys, 0))) then
		R.concat (xs, ys)
	    else 
		R.concat (ys, xs)
	else 
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
		val (ysL, ysR) = R.cut (ys, binarySearch cmp (lastElt xsL, ys))
	    in
		R.concat (| pMerge cmp (xsL, ysL), pMerge cmp (xsR, ysR) |)
	    end

    fun pMergesort cmp xs = 
	if R.isLeaf xs then
	    let
		val xs' = copy xs
		val R.LEAF (_, xsS') = xs'
	    in
		ArrayQSort.sort cmp xsS';
		xs'
	    end
	else if R.length xs <= 1 then
	    xs
	else if R.length xs = 2 then
	    if less (cmp (R.sub (xs, 0), R.sub (xs, 1))) then
		xs
	    else
		R.revP xs
	else
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
	    in
		pMerge cmp (| pMergesort cmp xsL, pMergesort cmp xsR |)
	    end

  end
