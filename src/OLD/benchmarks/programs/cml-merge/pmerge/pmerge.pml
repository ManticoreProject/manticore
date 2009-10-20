(* pmerge.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Merge two sorted arrays in parallel using O(n) work and O(log n) span. Therefore
 * the average parallelism is O(n / log n). 
 *
 *)

structure PMerge =
  struct

    fun less ord = 
	(case ord
	  of LESS => true
	   | _ => false)

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

  (* split ys in two lazily: those elements <= x and those >= x. the split is
   * arbitrary when for the elements of y that are equal to x.
   *)
    fun fsplit cmp (ys : 'a parray future, x : 'a) : ('a parray future * 'a parray future) =
	let val s = future (fn _ => R.cut (ys, binarySearch cmp (x, ys)))
	in
	    (future (fn _ => fst(touch s)),
	     future (fn _ => snd(touch s)))
	end

  (* We use futures to obtain the O(log n) span. If the futures (and touches) are 
   * erased, the span is O(log^2 n), since the merge would be performing the log(n) 
   * binary search for each of the log(n) recursive steps.
   *)

    fun pMerge (cmp : 'a * 'a -> ord) (xs : 'a parray, ys : 'a parray future)
                      : 'a parray =
	if R.length xs < R.length (touch ys) then
	    pMerge cmp (ys, xs)
	else if R.length xs = 0 orelse R.length (touch ys) = 0 then
	    xs
	else if R.length xs = 1 (* andalso R.length (touch ys) = 1 *) then
	    if less (cmp (R.sub (xs, 0), R.sub (touch ys, 0))) then
		R.concat (xs, touch ys)
	    else 
		R.concat (touch ys, xs)
	else 
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
		val (ysL, ysR) = fsplit cmp (ys, lastElt xsL)
	    in
		R.concat (| pMerge cmp (xsL, ysL), pMerge cmp (xsR, ysR) |)
	    end

  end

val _ = Print.printLn "TODO: implement Main.main"
