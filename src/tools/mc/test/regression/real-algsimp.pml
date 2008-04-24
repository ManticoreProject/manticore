fun b2s b = if b then "true" else "false";
val one = 1.0;
val zero = 0.0;
val posInf = one / zero;
val negInf = ~one / zero;

val nan1 = posInf + negInf;

fun cmp f = print ((b2s (f (nan1, nan1))) ^ "\n");

fun lt (x, y) = x < y;
fun lte (x, y) = x <= y;
fun gt (x, y) = x > y;
fun gte (x, y) = x >= y;

val _ = cmp lt;
val _ = cmp lte;
val _ = cmp gt;
val _ = cmp gte;

fun main _ = ()
