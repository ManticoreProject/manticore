(* this example fails in the BOM checker *)

fun compose (f, g) = let
    fun h (x) = f(g(x))
    in
        h
    end
;

fun fst (x, _) = x;
fun snd (_, y) = y;

fun filter (f, ls) = let
    fun loop arg = (case arg
        of (nil, res) => rev(res)
	 | (x :: xs, res) => loop(xs, if f(x) then x :: res else res)
        (* end case *))
    in
       loop(ls, nil)
    end
;

val xs = (1, true)  ::
	 (2, false) ::
	 (3, true)  ::
	 nil;

val odds = filter (snd, xs);
val evens = filter (compose(not, snd), xs);

()
