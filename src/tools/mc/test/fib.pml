fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

fun try (n : long) = let
      val f = fib(n)
      val s = Long.toString(f)
      in
        Print.print ("fib(" ^ Long.toString(n) ^ ") is " ^ s ^ ".\n")
      end;

val _ = List.app try (0::1::5::10::20::31::33::nil)
