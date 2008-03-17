(* batcher-bitonic-sort-ip.sml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Prototype for an in-place version of Batcher's bitonic sort.
 *)

structure BatcherBitonicSortInPlace : sig

    val run : int -> real

  end = struct

    val aupdate = Array.update
    val asub = Array.sub
    val alength = Array.length
    val array = Array.array
    type double = real

(*
    fun bubbleSort (arr) = let
	val n = alength(arr)
	fun swap (arr, i, j) = let
	    val t = asub(arr, i)
	    in
	       aupdate(arr, i, asub(arr, j));
	       aupdate(arr, j, t)
            end
	fun loop1 (i) = let
	    fun loop2 (j) = if (j >= i)
                then (
                      if (asub(arr, j-1) > asub(arr, j))
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
	    loop1(1)
         end
*)
    datatype dir = ASCENDING | DESCENDING

    fun compare (arr : double array, dir, i : int, j : int) = let
        val comp = Real.>(asub(arr, i), asub(arr, j))
        val exchange = (case dir
			 of ASCENDING => comp
			  | DESCENDING => not(comp)
                       (* end case *))
        in
	    if exchange
	        then let
		    val t = asub(arr, i)
		    in
		        aupdate(arr, i, asub(arr, j));
			aupdate(arr, j, t)
		    end
             else ()
        end

    fun lg (n:int) = let
	fun loop (x, y) = if (x = 1)
            then y
            else loop(x div 2, y + 1)
        in
           loop(n, 0)
        end

    fun sortingLoop (arr, dir, m, lo, i) = if (i < lo+m)
        then (compare(arr, dir, i, i+m);
	      sortingLoop(arr, dir, m, lo, i+1))
        else ()

    fun sortingNetwork (arr, dir, i, lo, m, lvl) = if (lvl > 0)
        then let
          val _ = sortingNetwork(arr, dir, 2*i, lo, m, lvl-1)
          val _ = sortingNetwork(arr, dir, 2*i+1, lo, m, lvl-1)
          in
             ()
          end
        else compare(arr, dir, i+lo, i+lo+m)

    fun bitonicMerge (arr, lo, n, dir) = if (n > 1)
        then let
          val m = n div 2
	  val _ = sortingNetwork(arr, dir, 0, lo, m, lg(m))
(*val _ = sortingLoop(arr, dir, m, lo, 0)*)
	  val _ = bitonicMerge(arr, lo,   m, dir)
	  val _ = bitonicMerge(arr, lo+m, m, dir)
          in
	      ()
          end
        else ()
(*
    val xs = [1,3,7,8,17,4,2,0]
    fun checkBitMerge () = let
	val xArr = Array.fromList xs
        in
	   bitonicMerge(xArr, 0, alength(xArr), ASCENDING);
	   Array.foldr (op ::) [] xArr
        end
*)
    fun batcherSort (arr, lo, n, dir) = if (n > 1)
        then let
           val m = n div 2
           in
                batcherSort (arr, lo,   m, ASCENDING);
                batcherSort (arr, lo+m, m, DESCENDING);
		bitonicMerge (arr, lo, n, dir)
           end
        else ()

(*
    val xs1 = [8,7,6,5,4,3,2,1]
    val xs1 = [
	1234,34,3,4,33,3432,334233,~4,~234,34,3,4,333,~3432,34233,~4,
	234,34,3,4,33,3432,34233,~4,234,34,3,4,333,~3432,34233,~4
    ]


    fun checkBatcherSort(ls) = let
	val arr = Array.fromList(ls)
	val arr' = copyArr(arr)
        in
	    batcherSort(arr, 0, alength(arr), ASCENDING);
	    bubbleSort(arr');
	    (Array.foldr (op ::) [] arr, Array.foldr (op ::) [] arr')
	    arrayEq(arr, arr')
        end
    val ys1 = checkBatcherSort(xs1)
*)

    val r = Random.rand(0, 1000)
    fun drand () = Random.randReal(r)
    val gettimeofday = Time.toReal o Time.now

    fun genRandomDoubleArr (n) = let
	val arr : double array = array(n, 0.0:double)
	fun loop (i) = if (i < n)
		       then (aupdate(arr, i, drand()); 
			     loop(i+1))
		       else ()
    in
	loop(0);
	arr
    end

fun pow2 (n) = if (n=0) then 1 else 2 * pow2(n-1)
;


    fun run (sz) = let
	val sz = pow2(sz)
	val arr = genRandomDoubleArr(sz)
	val b = gettimeofday()
	val _ = batcherSort(arr, 0, alength(arr), ASCENDING)
	val e = gettimeofday()
        in
	  e-b
        end

  end
