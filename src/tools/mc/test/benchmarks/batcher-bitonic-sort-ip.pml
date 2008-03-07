(* batcher-bitonic-sort-ip.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * In-place version of Batcher's bitonic sort.
 *)

datatype dir = ASCENDING | DESCENDING;

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
;

    fun compareLoop (arr, dir, m, lo, i) = if (i < lo+m)
	then (compare(arr, dir, i, i+m);
	      compareLoop(arr, dir, m, lo, i+1))
	else ()
;

    fun bitonicMerge (arr, lo, n, dir) = if (n > 1)
        then let
          val m = n div 2
	  val _ = compareLoop(arr, dir, m, lo, lo)
	  dval _ = bitonicMerge(arr, lo,   m, dir);
          in
	      bitonicMerge(arr, lo+m, m, dir)
          end
        else ()
;

    fun batcherSort (arr, lo, n, dir) = if (n > 1)
        then let
           val m = n div 2
	   dval _ = batcherSort (arr, lo,   m, ASCENDING)
	   val _ = batcherSort (arr, lo+m, m, DESCENDING)
           in                
		bitonicMerge (arr, lo, n, dir)
           end
        else ()
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

(* grab the input size x from stdin, generate a random array of length 2^x, 
 * and sort it.
 *)
fun timeTest () = let
    val x = readint()
    val n = pow2(x)

    val arr = genRandomDoubleArr(n)

(*    val _ = print (arr2s (dtos, arr)^"\n"); *)

    val b = gettimeofday ()
    val _ = batcherSort(arr, 0, alength(arr), ASCENDING)
    val e = gettimeofday ()
    in
(*        print (arr2s (dtos, arr)^"\n");*)
        print (dtos (e-b)^"\n")
    end
;

timeTest()
