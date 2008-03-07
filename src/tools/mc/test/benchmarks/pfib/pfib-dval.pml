(* find out how coarsening the granularity affects scalability
 *)

val gotoSeqSz = readint();

#include "pfib.h"
(* generate parallel and sequential versions of fib *)
seq_fib()
par_fib(gotoSeqSz)

val n = readint();
val b = gettimeofday ();
val s = itos (pfib(n));
val e = gettimeofday ();

print (dtos(e-b) ^ "\n")
