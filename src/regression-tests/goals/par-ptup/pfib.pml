fun add (m : long, n : long) = m + n;

fun pfib (i : long) = (case i
       of 0 => (0 : long)
	| 1 => (1 : long)
	| n => add (| pfib(i-1), pfib(i-2) |)
      (* end case *));

val _ = Print.printLn ("pfib(26) is " ^ (Long.toString (pfib 26)))

