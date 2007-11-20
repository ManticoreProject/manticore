fun f1 x = let fun g y = y <= x in g end;

fun filter (g, ls) = (case ls
    of nil => nil
     | x :: xs => if g x then x :: filter (g, xs)
                         else filter (g, xs)
    (* end case *))
;

fun floatListToString ls =  let
    fun loop (ls, acc) =  (case ls
        of nil => acc
	 | x :: nil => acc ^ (ftos x)
	 | x :: xs => loop (xs, acc ^ (ftos x) ^ ", ")
        (* end case *))
    in
       "[" ^ loop (ls, "") ^ "]"
    end
;

fun intListToString ls = let
    fun loop (ls, acc) =  (case ls
        of nil => acc
	 | x :: y :: nil => acc ^ (itos x) ^ ", " ^ (itos y)
	 | x :: xs => loop (xs, acc ^ (itos x) ^ ", ")
        (* end case *))
    in
       "[" ^ loop (ls, "") ^ "]"
    end
;

val x : float list = 5.0 :: ~1.0 :: 1002.123 :: 1.0 :: 20.0 :: nil;
val y = filter (f1 10.0, x);

val _ = print ((floatListToString x)^"\n");
print ((floatListToString y)^"\n")
