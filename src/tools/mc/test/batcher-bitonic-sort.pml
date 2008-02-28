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

(*
fun parrSplit (arr) = let
    val n = plen(arr)
    in
        (parrSubseq(arr, 0, n div 2), parrSubseq(arr, n div 2, n))
    end
;
*)

fun bitonicSort (arr) = if (plen(arr) = 1)
	  then arr
          else let
              val (bot, top) = pdivide(arr) (* parrSplit(arr) *)
	      (* FIXME I think bot/top need to be padded to same length b/c of zip semantics. -ams *)
	      val mins = [| min(b,t) | b in bot, t in top |]
	      val maxs = [| max(b,t) | b in bot, t in top |]
	      in
		   pappend(bitonicSort(mins), bitonicSort(maxs))
	      end
;

fun batcherSort (arr) = if (plen(arr) = 1)
          then arr
          else let
	      val (bot, top) = pdivide(arr) (* parrSplit(arr) *)
	      pval sortedBot = batcherSort(bot)
	      val  sortedTop = batcherSort(top)
	      in
		    bitonicSort(pappend(sortedBot, prev(sortedTop)))
	      end
;

val xs1 = [|0,1,2,19,10,0,1,6,~1|];
val ys1 = batcherSort(xs1);

print("result: " ^ parString ys1 ^ "\n")
