val (ns : int parray) = [| 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 |];
                        (* That's 100 total. *)

val s = sumP ns;

(print (itos s);
 print "\n")
