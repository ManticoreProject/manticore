(* The tak function is good for stress testing
 * function-call efficiency and should also test
 * the GC for large enough values.  But before you
 * try changing the arguments, check out the wiki entry
 * http://en.wikipedia.org/wiki/Tak_(function).  The number
 * of function calls becomes explosive at a point.
 *)

fun tak (x : long, y : long, z : long) = 
    if x <= y
       then y
       else tak ( tak (x-1, y, z),
		  tak (y-1, z, x),
		  tak (z-1, x, y) );
tak (15, 6, 1)
