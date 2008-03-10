(* batcher-bitonic-sort-ip.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * In-place version of Batcher's bitonic sort with sequential base cases for the 
 * functions containing dvals.
 *)

    fun lg (n) = let
	fun loop (x, y) = if (x = 1)
            then y
            else loop(x div 2, y + 1)
        in
           loop(n, 0)
        end
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
define({_SEQ_SZ_LG_}, {10})dnl
define({_SEQ_SZ_}, {1024})dnl
define({_SORTING_NETWORK_}, {
  define({_SORTING_NETWORK_FN_}, {$1})dnl
  define({_VAL_}, {$2})dnl
  define({_GOTO_SEQ_}, {$3})dnl
  define({_SEQ_FN_}, {$4})dnl
    fun _SORTING_NETWORK_FN_ (arr, dir, i, lo, m, lvl) = if (_GOTO_SEQ_)
        then _SEQ_FN_ (arr, dir, i, lo, m, lvl)
        else if (lvl > 0)
           then let
             _VAL_ _ = _SORTING_NETWORK_FN_ (arr, dir, 2*i, lo, m, lvl-1)
             val _ =   _SORTING_NETWORK_FN_ (arr, dir, 2*i+1, lo, m, lvl-1)
             in
                ()
             end
           else compare(arr, dir, i+lo, i+lo+m)
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
        then _SEQ_FN_ (arr, lo, n, dir)
        else if (n > 1)
           then let
             val m = n div 2
	     val _ = sortingNetwork(arr, dir, 0, lo, m, lg(m))
	     _VAL_ _ = _BITONIC_MERGE_FN_ (arr, lo,   m, dir)
             val _ = _BITONIC_MERGE_FN_ (arr, lo+m, m, dir)
             in
	         ()
             end
           else ()
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
        then _SEQ_FN_ (arr, lo, n, dir)
        else if (n > 1)
           then let
              val m = n div 2
	      _VAL_ _ = _BATCHER_SORT_FN_ (arr, lo,   m, ASCENDING)
	      val _ =   _BATCHER_SORT_FN_ (arr, lo+m, m, DESCENDING)
              in                
		   bitonicMerge (arr, lo, n, dir)
              end
           else ()
;
})dnl
_BATCHER_SORT_({seqBatcherSort}, {val}, {false}, {seqBatcherSort})
_BATCHER_SORT_({batcherSort}, {dval}, {(n < _SEQ_SZ_)}, {seqBatcherSort})

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

ifdef({DEBUG}, {
    val _ = print (arr2s (dtos, arr)^"\n"); 
})dnl

    val b = gettimeofday ()
    val _ = batcherSort(arr, 0, alength(arr), ASCENDING)
    val e = gettimeofday ()
    in
ifdef({DEBUG}, {
        print (arr2s (dtos, arr)^"\n");
})dnl
        print (dtos (e-b)^"\n")
    end
;

timeTest()
