fun fact (n : long) = let
      fun fact' (i, acc) = if (i <= n)
	    then fact' (i+1, i*acc)
	    else acc
      in
	fact' (1, 1)
      end

fun try (n : long) = let
      val f = fact(n)
      val s = Long.toString(f)
      in
        Print.print ("fact(" ^ Long.toString(n) ^ ") is " ^ s ^ ".\n")
      end;

val _ = List.app try (0::1::5::10::20::nil)
