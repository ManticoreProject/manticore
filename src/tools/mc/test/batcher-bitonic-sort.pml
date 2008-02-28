(* Parallel version of Batcher's bitonic sort
 * 
 * New operators we need to get this example working:
 *   1. parrSubseq(arr,lo,hi)    returns the parray [| arr[lo], ..., arr[hi-1] |]   *or*
 *      parrSplit(arr)           returns ([| arr[0], ... arr[n/2] |], [| arr[n/2], ... arr[n] |])
 *   2. parrConcat(arr1,arr2)    concatenates arr1 and arr2 (we'll need this to be log time and balancing for benchmarks)
 *   3. parrRev(arr)             reverse arr
 *)

fun parr2ls (a) = let
    val len = plen a
    fun build (curr, acc) =
        if curr=len
           then rev acc
           else build (curr+1, (itos (a!curr)) :: acc)
    in
       build(0, nil)
    end
;

fun parrSplit (arr) = let
    val n = plen(arr)
    in
        (parrSubseq(arr, 0, n div 2), parrSubseq(arr, n div 2, n))
    end
;

fun bitonicSort (arr) = if (plen(arr) = 1)
	  then arr
          else let
              val (bot, top) = parrSplit(arr)
	      val mins = [| min(b,t) | x in bot, y in top |]
	      val mins = [| max(b,t) | x in bot, y in top |]
	      in
		   parrConcat(bitonicSort(mins), bitonicSort(maxs))
	      end
;

fun batcherSort (arr) = if (plen(arr) = 1)
          then arr
          else let
	      val (bot, top) = parrSplit(arr)
	      pval sortedBot = batcherSort(bot)
	      val sortedTop  = batcherSort(top)
	      in
		    bitonicSort(parrConcat(sortedBot, parrRev(sortedTop)))
	      end
;

val xs1 = [0,1,2,19,10,0,1,6,~1];
val ys1 = batcherSort(xs1);

print("result: "^parr2ls ys1^"\n")
