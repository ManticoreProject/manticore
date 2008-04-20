fun fib (i : long) = (case i
       of 0 => 0
	| 1 => 1
	| n => fib(i-1) + fib(i-2)
      (* end case *));

fun try (n : long) = let
      val f = fib(n)
      val s = ltos(f)
      in
        print ("fib(" ^ ltos(n) ^ ") is " ^ s ^ ".\n")
      end;

app (try, 0::1::5::10::20::31::33::nil)
