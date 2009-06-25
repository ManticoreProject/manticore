(* this example fails in the BOM checker *)

fun snd (_, y) = y;

val xs = (1,true)::nil;

val odds = filter (snd, xs);

()
