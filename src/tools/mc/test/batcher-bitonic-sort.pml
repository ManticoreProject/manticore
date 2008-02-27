(* Parallel version of Batcher's bitonic sort
 * 
 * New operators we need to get this example working:
 *   1. parrSubseq(arr,lo,hi)    returns the parray [a[lo], ..., a[hi-1]]
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


fun bitonicSort (arr) = let
    val n = plen(arr)
    in
       if (n = 1)
	  then arr
          else let
              val bot = parrSubseq(arr, 0, n div 2)
	      val top = parrSubseq(arr, n div 2, n)
	      val mins = [| min(b,t) | x <- bot, y <- top |]
	      val mins = [| max(b,t) | x <- bot, y <- top |]
	      in
		   parrConcat(bitonicSort(mins), bitonicSort(maxs))
	      end
    end
;

fun batcherSort (arr) = let
    val n = plen
    in
       if (n = 1)
          then arr
          else let
	      pval bot = batcherSort(parrSubseq(arr, 0, n div 2))
	      pval top = batcherSort(parrSubseq(arr, n div 2, n))
	      in
		    bitonicSort(parrConcat(bot, parrRev(top)))
	      end
    end
;

val xs1 = [0,1,2,19,10,0,1,6,~1];
val ys1 = batcherSort(xs1);

print("result: "^parr2ls ys1^"\n")
