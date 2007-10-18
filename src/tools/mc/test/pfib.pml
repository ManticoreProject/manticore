fun add (m : long, n : long) = m + n;

fun pfib (i : long) = (case i
       of 0 => (0 : long)
	| 1 => (1 : long)
	| n => let val (x,y) =  (| pfib(i-1), pfib(i-2) |)
               in  x+y end
      (* end case *));

val s = ltos (pfib 18);

val _ = print ("\npfib(20) is " ^ s ^ "\n");

()
