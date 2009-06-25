fun g n = n - 1;

val h = compose(Int.toString, g);

val _ = Print.print ("The result is " ^ h(10) ^ " (expected 9).\n")
