(* find out how coarsening the granularity affects scalability
 *)

fun fib i = (case i
       of 0 => 0
	| 1 => 1
	| n => let
         val x = fib(i-1)
         val y = fib(i-2)
         in
	      x + y
	 end
      (* end case *));

val gotoSeqSz = readint();

(* parallel fib that cuts to sequential version at fib(gotoSeqSz) *)
fun pfib (i) = if (i < gotoSeqSz)
    then fib(i)
    else
    (case i
       of 0 => 0
	| 1 => 1
	| n => let
         dval x = pfib(i-1)
         val y = pfib(i-2)
         in
	      x + y
	 end
      (* end case *));

val n = readint();
val b = gettimeofday ();
val s = itos (pfib(n));
val e = gettimeofday ();

print (dtos(e-b) ^ "\n")
