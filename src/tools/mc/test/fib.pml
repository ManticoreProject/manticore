fun fib (i : int) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

fun try (n : int) = let
      val f = fib(n)
      val s = Int.toString(f)
      in
        Print.print ("fib(" ^ Int.toString(n) ^ ") is " ^ s ^ ".\n")
      end;

val _ = List.app try (0::1::5::10::20::31::33::nil)
