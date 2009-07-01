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
	 | x :: nil => acc ^ (Float.toString x)
	 | x :: xs => loop (xs, acc ^ (Float.toString x) ^ ", ")
        (* end case *))
    in
       "[" ^ loop (ls, "") ^ "]"
    end
;

fun intListToString ls = let
    fun loop (ls, acc) =  (case ls
        of nil => acc
	 | x :: y :: nil => acc ^ (Int.toString x) ^ ", " ^ (Int.toString y)
	 | x :: xs => loop (xs, acc ^ (Int.toString x) ^ ", ")
        (* end case *))
    in
       "[" ^ loop (ls, "") ^ "]"
    end
;

val x : float list = 5.0 :: ~1.0 :: 1002.123 :: 1.0 :: 20.0 :: nil;
val y = filter (f1 10.0, x);

val _ = Print.print ((floatListToString x)^"\n");
val _ = Print.print ((floatListToString y)^"\n")
