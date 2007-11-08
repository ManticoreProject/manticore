fun g n = n - 1;

val h = compose(itos, g);

print ("The result is " ^ h(10) ^ " (expected 9).\n")