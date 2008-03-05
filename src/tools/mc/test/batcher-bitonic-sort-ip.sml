(* batcher-bitonic-sort-ip.sml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Prototype for an in-place version of Batcher's bitonic sort.
 *)
oooooooooooooooooooooooooooooooooooo
structure BatcherBitonicSortInPlace = 
  struct

    val aupdate = Array.update
    val asub = Array.sub
    val alength = Array.length
    val array = Array.array

    datatype dir = ASCENDING | DESCENDING

    fun bitonicMerge (arr, lo, n, dir) = if (n > 1)
        then let
          val m = n div 2
	  fun compare (i, j) = let
              val comp = asub(arr, i) > asub(arr, j)
              val exchange = if (dir=ASCENDING)
                  then comp
                  else not(comp)
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
          fun compareLoop (i) = if (i < lo+m)
              then (compare(i, i+m);
		    compareLoop(i+1))
              else ()
          in
              compareLoop(lo);
	      bitonicMerge(arr, lo,   m, dir);
	      bitonicMerge(arr, lo+m, m, dir)
          end
        else ()

    val xs = [1,3,7,8,17,4,2,0]
    fun checkBitMerge () = let
	val xArr = Array.fromList xs
        in
	   bitonicMerge(xArr, 0, alength(xArr), ASCENDING);
	   Array.foldr (op ::) [] xArr
        end

    fun batcherSort (arr, lo, n, dir) = if (n > 1)
        then let
           val m = n div 2
           in
                batcherSort (arr, lo,   m, ASCENDING);
                batcherSort (arr, lo+m, m, DESCENDING);
		bitonicMerge (arr, lo, n, dir)
           end
        else ()

    val xs1 = [8,7,6,5,4,3,2,1]
    val xs1 = [0,1,2,19,10,0,1,~1]
    fun checkBatcherSort(ls) = let
	val arr = Array.fromList(ls)
        in
	    batcherSort(arr, 0, alength(arr), ASCENDING);
	    Array.foldr (op ::) [] arr
        end
    val ys1 = checkBatcherSort(xs1)

  end
