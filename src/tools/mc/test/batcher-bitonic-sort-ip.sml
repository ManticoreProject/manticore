(* batcher-bitonic-sort-ip.sml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Prototype for an in-place version of Batcher's bitonic sort.
 *)

structure BatcherBitonicSortInPlace = 
  struct

    val aupdate = Array.update
    val asub = Array.sub
    val alength = Array.length
    val array = Array.array

    datatype dir = ASCENDING | DESCENDING

    fun compare (arr, dir, i, j) = let
        val comp = asub(arr, i) > asub(arr, j)
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

    fun lg (n) = let
	fun loop (x, y) = if (x = 1)
            then y
            else loop(x div 2, y + 1)
        in
           loop(n, 0)
        end

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
	  val _ = bitonicMerge(arr, lo,   m, dir)
	  val _ = bitonicMerge(arr, lo+m, m, dir)
          in
	      ()
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
