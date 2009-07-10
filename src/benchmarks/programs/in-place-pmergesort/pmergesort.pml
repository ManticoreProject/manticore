(* pmergesort.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel mergesort adapted from the Cilk5 benchmarks.
 *)

structure PMergesort =
  struct

    structure A = Array64

    fun pow2 n = if (n=0) then 1 else 2 * pow2(n-1)

    fun copyArr arr2 = let
	val arr1 = A.array(A.length arr2, A.sub(arr2, 0))
	fun loop i = if (i > 0)
            then (
               A.update(arr1, i, A.sub(arr2, i));
	       loop(i-1))
            else ()
        in
	   loop(A.length(arr1)-1);
	   arr1
        end


    val epsilon = 0.01
    val abs = Double.abs

  (* returns true of the arrays are equal (within some threshold) *)
    fun arrayEq (arr1, arr2) = let
	fun loop (i) = if (i > 0)
            then (
               if (abs(A.sub(arr1, i) - A.sub(arr2, i)) <= epsilon)
                  then loop(i-1)
                  else false)
            else true
        in
	   loop(A.length(arr1)-1)	   
        end

    fun bubbleSort (arr) = let
	val n = A.length(arr)
	fun swap (arr, i, j) = let
	    val t = A.sub(arr, i)
	    in
	       A.update(arr, i, A.sub(arr, j));
	       A.update(arr, j, t)
            end
	fun loop1 i = let
	    fun loop2 j = if (j >= i)
                then (
                      if (A.sub(arr, j-1) > A.sub(arr, j))
                         then swap (arr, j, j-1)
                         else ();
		      loop2(j-1))
                else ()
            in
               if (i < n)
		  then (
		   loop2(n-1);
		   loop1(i+1))
	          else ()
            end
         in
	    loop1 1
         end

    fun b2s (b) = if b then "true" else "false"

    fun len (_, s1, s2) = s2-s1


    fun maxval (x, y) = if x > y then x else y

    (* assume that b>a. *)
    fun binarySearch' (arr, a, b, x) = if (b = a)
        then a
        else let
          val p = (b+a) div 2
          val (a, b) = if (A.sub(arr,p) < x)
		          then (p+1, b)
		          else (a,   p)
          in
	      binarySearch'(arr, a, b, x)
          end


   (* find j such that arr[j] <= x <= arr[j+1] *)
    fun binarySearch (arr, a, b, x) = let
	val (a, b) = if (a < b) then (a, b) else (b, a)
        in
	    binarySearch' (arr, a, b, x)
        end

   (* copy l into d *)
    fun copy ( (dArr, d1, d2), (lArr, l1, l2) ) = let
	    val l = (lArr, l1, l2)
	    fun loop (i) = if (i >= 0)
                then (A.update(dArr, d1+i, A.sub(lArr, l1+i));
		      loop(i-1))
                else ()
            in
	       loop(len(l)-1)
	    end

    fun pMerge' (d, l, r) = (
	case (d, l, r)
 	 of ((dArr, d1, d2), (lArr, l1, l2), (rArr, r1, r2)) =>
	    if (len(l) < len(r))
	    then pMerge' (d, r, l)
	    else if (len(l) = 0 orelse len(r) = 0)
	    then (copy(d, l) ; 0)
	    else if (len(l) = 1)
	    then if (A.sub(lArr, l1) < A.sub(rArr, r1)) 
		 then (A.update(dArr, d1,   A.sub(lArr, l1)); 
		       A.update(dArr, d1+1, A.sub(rArr, r1));
		       0) 
		 else (A.update(dArr, d1,   A.sub(rArr, r1)); 
		       A.update(dArr, d1+1, A.sub(lArr, l1));
		       0)
		      
	    else let
	            val lLen' = len(l) div 2
		    val j = binarySearch(rArr, r1, r2, A.sub(lArr, lLen' + l1))
		    val rLen' = j - r1
		    pval c1 = pMerge' ( (dArr, d1, lLen' + rLen' + d1), 
					(lArr, l1, lLen' + l1), 
					(rArr, r1, j) )
		    val c2 = pMerge' ( (dArr, lLen' + rLen' + d1, d2), 
				       (lArr, lLen' + l1, l2), 
					   (rArr, j, r2) )
		in
		    c1+c2
		end
        (* end case *))


   (* merge sorted arrays arr[p..q] and arr[q..r] into the sorted array dArr[p..r] *)
    fun pMerge (dArr, arr, p, q, r) = 
	pMerge'( (dArr, p, r), (arr, p, q), (arr, q, r) )

    fun pMergesort' (dArr, dArr', arr, p, r) = if (r-p > 1)
           then let
             val q = (p+r) div 2
             pval x = pMergesort' (dArr', dArr, arr, p, q)
             val y = pMergesort' (dArr', dArr, arr, q, r)
	     val xy = x+y
	     val z = pMerge(dArr, dArr', p, q, r)
             in
                xy+z
             end
        else (A.update(dArr, p, A.sub(arr, p)); 0)

   (* parallel merge sort *)
    fun pMergesort (arr) = let
	val dArr = A.array(A.length(arr), A.sub(arr,0))
        val dArr' = A.array(A.length(arr), A.sub(arr,0))
	in
	   pMergesort'(dArr, dArr', arr, 0, A.length(arr));
	   dArr
        end

    fun arr2s (elt2s, arr) = let
	val n = A.length(arr)
	fun loop (i, str) = if (i >= 0)
	    then loop(i-1, elt2s (A.sub(arr, i))^", "^str)
	    else str
	in
	    "["^loop(n-1, "")^"]"
	end
    ;

    fun genRandomDoubleArr (n) = let
	val arr : double A.array = A.array(n, 0.0:double)
	fun loop (i) = if (i < n)
	    then (A.update(arr, i, Rand.randDouble(0.0:double, 100.0:double)); 
		  loop(i+1))
	    else ()
	in
	   loop(0);
	   arr
	end
    ;

  (* check parallel mergesort against bubble sort *)
    fun debug () = let
	val n = PrimIO.readInt()

	val arr = genRandomDoubleArr(n)
	val arr' = copyArr(arr)

	val (arr, t) = Time.timeToEval(fn () => pMergesort(arr))

	val _ = bubbleSort(arr')
(*	val _ = print (arr2s (dtos, arr)^"\n"); 
	val _ = print (arr2s (dtos, arr')^"\n")
*)
	in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    if arrayEq(arr, arr')
	       then ()
	    else Print.printLn "arrays not equal!"
	end

(*    val _ = debug()*)

  (* benchmark parallel mergesort *)
    fun bench () = let
	val n = PrimIO.readInt()

	val arr = genRandomDoubleArr(n)

	val (arr, t) = Time.timeToEval(fn () => pMergesort(arr))
	in
	    Print.printLn("Time elapsed (microseconds): "^Long.toString t);
	    ()
	end

    val _ = bench()

  end
