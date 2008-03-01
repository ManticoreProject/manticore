(* Parallel version of Batcher's bitonic sort
 * 
 * New operators we need to get this example working:
 *   1. parrSubseq(arr,lo,hi)    returns the parray [| arr[lo], ..., arr[hi-1] |]   *or*
 *      parrSplit(arr)           returns ([| arr[0], ... arr[n/2] |], [| arr[n/2], ... arr[n] |])
 *)

fun max (m:int, n) = (if m>n then m else n);

fun min (m:int, n) = (if m>n then n else m);

fun parString a =
  let val len = plen a
      fun build (curr, acc) =
        if curr=len
        then rev acc
        else build (curr+1, (itos (a!curr)) :: acc)
  in
      "[|" ^ (concatWith (",", build (0, nil))) ^ "|]"
  end;

fun bitonicSort (arr) = if (plen(arr) = 1)
	  then arr
          else let
              val (bot, top) = pdivide(arr)
	      val mins = [| min(b,t) | b in bot, t in top |]
	      val maxs = [| max(b,t) | b in bot, t in top |]
	      in
		   pappend(bitonicSort(mins), bitonicSort(maxs))
	      end
;

fun batcherSort (arr) = if (plen(arr) = 1)
          then arr
          else let
	      val (bot, top) = pdivide(arr)
	      val sortedBot = batcherSort(bot)
	      val sortedTop = batcherSort(top)
	      in
		  bitonicSort(pappend(sortedBot, prev(sortedTop)))
	      end
;

val xs1 = [| 2, 1 |];
val ys1 = batcherSort(xs1);

print("result: " ^ parString ys1 ^ "\n")
