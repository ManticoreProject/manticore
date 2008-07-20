(* batcher-bitonic-sort-ip.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * In-place version of Batcher's bitonic sort with sequential base cases for the 
 * functions containing dvals.
 *  INPUT:  takes two arguments from stdin: 1. sequential base case size; 2. log-sized input.
 *     i.e.,    echo "1024 15"|./a.out       (sequential base case of 1024 and array of size 2^15)
 *)

    fun copyArr (arr2) = let
	val arr1 = array(alength(arr2), asub(arr2, 0))
	fun loop (i) = if (i > 0)
            then (
               aupdate(arr1, i, asub(arr2, i));
	       loop(i-1))
            else ()
        in
	   loop(alength(arr1)-1);
	   arr1
        end
;

    val epsilon = 0.01;
    val abs = absd;

    fun arrayEq (arr1, arr2) = let
	fun loop (i) = if (i > 0)
            then (
               if (abs(asub(arr1, i) - asub(arr2, i)) <= epsilon)
                  then loop(i-1)
                  else false)
(*
(print(itos i^" "^dtos(asub(arr1, i))^" "^dtos(asub(arr2, i))^"\n");
loop(i-1)))
*)
            else true
        in
	   loop(alength(arr1)-1)	   
        end
;

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
;

fun b2s (b) = if b then "true" else "false";

    fun lg (n) = let
	fun loop (x, y) = if (x = 1)
            then y
            else loop(x div 2, y + 1)
        in
           loop(n, 0)
        end
;

fun pow2 (n) = if (n=0) then 1 else 2 * pow2(n-1)
;

    datatype dir = ASCENDING | DESCENDING
;

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

divert(-1)
changequote({,})   #change quotes to curly braces 
divert(0)

val seqSz = readint();
val seqSzP2 = pow2(seqSz);
define({_SEQ_SZ_LG_}, {seqSz})dnl
define({_SEQ_SZ_}, {seqSzP2})dnl

fun compareLoop (arr, dir, lo, m, i) = if (i < lo+m)
              then (compare(arr, dir, i, i+m);
		    compareLoop(arr, dir, lo, m, i+1))
              else ()
;

define({_SORTING_NETWORK_}, {
  define({_SORTING_NETWORK_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _SORTING_NETWORK_FN_ (arr, dir, i, lo, m, lvl) = if (_GOTO_SEQ_)
        then ( _SEQ_FN_ (arr, dir, i, lo, m, lvl); 0)
        else if (lvl > 0)
           then let
             _VAL_ x = _SORTING_NETWORK_FN_ (arr, dir, 2*i, lo, m, lvl-1)
             val y =   _SORTING_NETWORK_FN_ (arr, dir, 2*i+1, lo, m, lvl-1)
             in
                x+y
             end
           else (compare(arr, dir, i+lo, i+lo+m); 0)
;
})dnl
_SORTING_NETWORK_({seqSortingNetwork}, {val}, {false}, {seqSortingNetwork})
_SORTING_NETWORK_({sortingNetwork}, {dval}, {(lg(m)-lvl) < _SEQ_SZ_LG_}, {seqSortingNetwork})

define({_BITONIC_MERGE_}, {
  define({_BITONIC_MERGE_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _BITONIC_MERGE_FN_ (arr, lo, n, dir) = if (_GOTO_SEQ_)
        then (_SEQ_FN_ (arr, lo, n, dir); 0)
        else if (n > 1)
           then let
             val m = n div 2
	     val _ = sortingNetwork(arr, dir, 0, lo, m, lg(m))
	     _VAL_ x = _BITONIC_MERGE_FN_ (arr, lo,   m, dir)
             val y = _BITONIC_MERGE_FN_ (arr, lo+m, m, dir)
             in
	         x+y
             end
           else 0
;
})dnl
_BITONIC_MERGE_({seqBitonicMerge}, {val}, {false}, {seqBitonicMerge})
_BITONIC_MERGE_({bitonicMerge}, {dval}, {(n < _SEQ_SZ_)}, {seqBitonicMerge})

define({_BATCHER_SORT_}, {
  define({_BATCHER_SORT_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _BATCHER_SORT_FN_ (arr, lo, n, dir) = if (_GOTO_SEQ_)
        then (_SEQ_FN_ (arr, lo, n, dir); 0)
        else if (n > 1)
           then let
              val m = n div 2
	      _VAL_ x = _BATCHER_SORT_FN_ (arr, lo,   m, ASCENDING)
	      val y =   _BATCHER_SORT_FN_ (arr, lo+m, m, DESCENDING)
	      val xy = x+y
	      val z = bitonicMerge (arr, lo, n, dir)
            in        
        	xy+z
            end
           else 0
;
})dnl
_BATCHER_SORT_({seqBatcherSort}, {val}, {false}, {seqBatcherSort})
_BATCHER_SORT_({batcherSort}, {dval}, {(n < _SEQ_SZ_)}, {seqBatcherSort})

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

fun debug () = let
    val x = readint()
    val n = pow2(x)

    val arr = genRandomDoubleArr(n)
    val arr' = copyArr(arr)

    val b = gettimeofday ()
    val _ = batcherSort(arr, 0, alength(arr), ASCENDING)
    val e = gettimeofday ()

    val _ = bubbleSort(arr')
(*    val _ = print (arr2s (dtos, arr')^"\n"); *)
    in
(*        print (arr2s (dtos, arr)^"\n");*)
        print (dtos (e-b)^"\n");
        print (b2s(arrayEq(arr, arr'))^"\n")
    end
;

(* grab the input size x from stdin, generate a random array of length 2^x, 
 * and sort it.
 *)
fun timeTest () = let
    val x = readint()
    val n = pow2(x)

    val arr = genRandomDoubleArr(n)

    val b = gettimeofday ()
    val _ = batcherSort(arr, 0, alength(arr), ASCENDING)
    val e = gettimeofday ()
    in
        print (dtos (e-b)^"\n")
    end
;

ifdef({DEBUG}, {
val _ = debug();
})dnl
timeTest()
