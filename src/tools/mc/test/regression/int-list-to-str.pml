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

fun tabulate (f, lo, hi) = let
    fun loop (i, acc) = if (i < hi)
        then f(i) :: loop(i+1, acc)
        else rev(acc)
    in
       loop(lo, nil)
    end;

fun f i = i;
val ls = tabulate (f, 0, 700);
print (intListToString ls^"\n")
