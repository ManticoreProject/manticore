(* batcher-bitonic-sort-ip.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * In-place version of Batcher's bitonic sort.
 *)

datatype dir = ASCENDING | DESCENDING;

    fun bitonicMerge (arr, lo, n, dir) = if (n > 1)
        then let
          val m = n div 2
	  fun compare (i, j) = let
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
;

    fun batcherSort (arr, lo, n, dir) = if (n > 1)
        then let
           val m = n div 2
           in
                batcherSort (arr, lo,   m, ASCENDING);
                batcherSort (arr, lo+m, m, DESCENDING);
		bitonicMerge (arr, lo, n, dir)
           end
        else ()
;

fun genRandomDoubleArr (n) = let
    val arr : double array = array(n, 0.0:double)
    fun loop (i) = if (i < n)
        then (aupdate(arr, i, drand(0.0:double, 100.0:double)); 
	      loop(i+1))
        else ()
    in
       loop(0);
       arr
    end
;

fun pow2 (n) = if (n=0) then 1 else 2 * pow2(n-1);

fun arr2s (elt2s, arr) = let
    val n = alength(arr)
    fun loop (i, str) = if (i >= 0)
        then loop(i-1, elt2s (asub(arr, i))^", "^str)
        else str
    in
        "["^loop(n-1, "")^"]"
    end
;

fun timeTest () = let
    val x = readint()
    val n = pow2(x)

    val arr = genRandomDoubleArr(n)

    val _ = print (arr2s (dtos, arr)^"\n");

    val b = gettimeofday ()
    val _ = batcherSort(arr, 0, alength(arr), ASCENDING)
    val e = gettimeofday ()
    in
        print (arr2s (dtos, arr)^"\n");
        print (dtos (e-b)^"\n")
    end
;

timeTest()
