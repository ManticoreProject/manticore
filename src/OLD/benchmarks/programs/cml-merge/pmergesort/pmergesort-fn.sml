(* pmergesort-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Purely functional parallel mergesort.
 *)

functor PMergesortFn (
    structure K : ORD_KEY
    structure R : ROPE
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

    fun less ord = 
	(case ord
	  of LESS => true
	   | _ => false)

    fun lastElt arr = R.sub(arr, R.length arr - 1)

  (* return p such that xs[p] <= y <= xs[p+1] *)
  (* precondition: xs is sorted *)
    fun binarySearch (y, xs) = 
	let
	    fun lp (a, b) =
	        if b = a then
		    a
		else
		    let
			val p = (b + a) div 2
			val (a, b) = 
			    if less (K.compare (R.sub (xs, p), y)) then
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
    fun pMerge (xs, ys) =
	if R.length xs < R.length ys then
	    pMerge (ys, xs)
	else if R.length xs = 0 orelse R.length ys = 0 then
	    xs
	else if R.length xs = 1 (* andalso R.length ys = 1 *) then
	    if less (K.compare (R.sub (xs, 0), R.sub (ys, 0))) then
		R.append (xs, ys)
	    else 
		R.append (ys, xs)
	else 
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
	      (* note: currently pmergesort is not stable due to the choice of
	       * partition below, since ysL may contain some elements equal to 
	       * lastElt xsL. one possible fix is to use another binary search to
	       * ensure that ysL contains those elements < (lastElt xsL)
	       *)
		val (ysL, ysR) = R.cut (ys, binarySearch (lastElt xsL, ys))
	    in
		R.append ( pMerge (xsL, ysL), pMerge (xsR, ysR) )
	    end
	    
    fun pMergesort xs = 
	if R.length xs <= 1 then
	    xs
	else if R.length xs = 2 then
	    if less (K.compare (R.sub (xs, 0), R.sub (xs, 1))) then
		xs
	    else
		R.rev xs
	else
	    let
		val (xsL, xsR) = R.splitAt (xs, R.length xs div 2 - 1)
	    in
		pMerge ( pMergesort xsL, pMergesort xsR )
	    end

  end

structure Main =
  struct

    structure R = RopeFn (
	structure S = ListSeq 
	val maxLeafSize= 2)

    structure PMergesort = PMergesortFn (
	        structure K = struct
		   type ord_key = Int.int
		   val compare  = Int.compare
		end
		structure R = R)

    val dfltN = 100000

    fun timeit n = 
	let
	    val t0 = Time.now()
	    val x = R.tabulate(n, fn _ => Rand.range (0, 10000) 0w43343)
	    val _ = PMergesort.pMergesort x
	    val t = Time.-(Time.now(), t0)
	in
	    TextIO.print(Time.toString t)
	end

    fun main (_, args) = let
	  val n = (case args
		 of arg::_ => Option.getOpt (Int.fromString arg, dfltN)
		  | _ => dfltN
		(* end case *))
	  in
	    timeit n;
	    OS.Process.success
	  end
	
  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
