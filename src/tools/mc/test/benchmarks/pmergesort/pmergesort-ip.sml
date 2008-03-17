(* pmergesort-ip.sml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Prototype for an in-place version of parallel mergesort.
 *)

structure PMergesortInPlace : sig

    type double = real

    val pMergesort : double array -> double array

  end = struct

    val aupdate = Array.update
    val asub = Array.sub
    val alength = Array.length
    val array = Array.array
    type double = real

    fun len (_, s1, s2) = s2-s1

    (* assume that b>a. *)
    fun binarySearch' (arr, a, b, x : double) = if (b = a)
        then a
        else let
          val p = (b+a) div 2
          val (a, b) = if (asub(arr,p) < x)
		          then (p+1, b)
		          else (a,   p)
          in
	      binarySearch'(arr, a, b, x)
          end

   (* find j such that arr[j] <= x <= arr[j+1] *)
    fun binarySearch (arr: double array, a, b, x) = let
	val (a, b) = if (a < b) then (a, b) else (b, a)
        in
	    binarySearch' (arr, a, b, x)
        end

   (* copy l into d *)
    fun copy ( d as (dArr, d1, d2), l as (lArr, l1, l2) ) = let
	    fun loop (i) = if (i >= 0)
                then (aupdate(dArr, d1+i, asub(lArr, l1+i));
		      loop(i-1))
                else ()
            in
	       loop(len(l)-1)
	    end

   (* merge sorted arrays arr[p..q] and arr[q..r] into the sorted array dArr[p..r] *)
    fun pMerge (dArr : double array, arr, p, q, r) = let	
	fun loop ( d as (dArr, d1, d2), l as (lArr, l1, l2), r as (rArr, r1, r2) ) =
            if (len(l) < len(r))
	       then loop(d, r, l)
	    else if (len(l) = 0 orelse len(r) = 0)
	       then copy(d, l) 
	    else if (len(l) = 1)
	       then if (asub(lArr, l1) < asub(rArr, r1)) 
		       then (aupdate(dArr, d1,   asub(lArr, l1)); 
			     aupdate(dArr, d1+1, asub(rArr, r1))) 
		       else (aupdate(dArr, d1,   asub(rArr, r1)); 
			     aupdate(dArr, d1+1, asub(lArr, l1))) 
	    else let
               val lLen' = len(l) div 2
	       val j = binarySearch(rArr, r1, r2, asub(lArr, lLen' + l1)) 
	       val rLen' = j - r1
	       val c1 = loop( (dArr, d1, lLen' + rLen' + d1), (lArr, l1, lLen' + l1), (rArr, r1, j) )
	       val c2 = loop( (dArr, lLen' + rLen'+ d1, d2), (lArr, lLen' + l1, l2), (rArr, j, r2) )
	       in
		    ()
	       end
        in
	   loop( (dArr, p, r), (arr, p, q), (arr, q, r) )
	end

    fun pMergesort' (dArr: double array, dArr', arr, p, r) = if (r-p > 1)
        then let
          val q = (p+r) div 2
          val _ = pMergesort' (dArr', dArr, arr, p, q)
          val _ = pMergesort' (dArr', dArr, arr, q, r)
          in
             pMerge(dArr, dArr', p, q, r)
          end
        else aupdate(dArr, p, asub(arr, p)) 

   (* parallel merge sort *)
    fun pMergesort (arr : double array) = let
	val dArr = array(alength(arr), asub(arr,0))
        val dArr' = array(alength(arr), asub(arr,0))
	in
	   pMergesort'(dArr, dArr', arr, 0, alength(arr));
	   dArr
        end

  end

structure PMEx : sig

    val run : int -> real

  end = struct

    open PMergesortInPlace

    val r = Random.rand(0, 1000)
    fun drand () = Random.randReal(r)
    val gettimeofday = Time.toReal o Time.now

    fun genRandomDoubleArr (n) = let
	val arr : double array = Array.array(n, 0.0:double)
	fun loop (i) = if (i < n)
		       then (Array.update(arr, i, drand()); 
			     loop(i+1))
		       else ()
    in
	loop(0);
	arr
    end
(*
    fun mergeEx () = let
	val xs = [1,2,10,11,101]
	val ys = [~2,5,9,100,102]
	val n1 = 1
	val nzs = List.tabulate (n1, fn _ => 0)
	val arr = Array.fromList(nzs@xs@ys)
	val n = alength(arr)
	val dArr = Array.array (n, 0)
        in
          pMerge(dArr, arr, n1, List.length(xs) + n1, n);
	  Array.foldr (op ::) [] dArr
        end
*)
(*    fun sortEx () = let
	val xs = [20,123,~24,232222,183834,34234324,332,343,10000,5,3,~100,67,2,1,~12,
		  1022,3333333,~1,1,20,~20,100,1001,34,311,3,122,21,~1232]
	val arr = pMergesort(Array.fromList(xs))
	in
	  Array.foldr (op ::) [] arr	    
	end    
*)
    fun run (sz) = let
	val arr = genRandomDoubleArr(sz)
	val b = gettimeofday()
	val _ = pMergesort(arr)
	val e = gettimeofday()
        in
	  e-b
        end

  end
