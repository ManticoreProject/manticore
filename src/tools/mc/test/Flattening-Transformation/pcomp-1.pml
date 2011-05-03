val itos = Int.toString
fun for rng (f : int -> unit) = PArray.app f rng

val arr = [| x+1 | x in [| 1 to 20 |] |]

val _ = Print.printLn "starting...should see 2 through 21..."

fun pr i = Print.printLn ("expecting " ^ itos(i+2) ^ ": " ^ itos(arr!i))
val _ = for [| 0 to 19 |] pr

val _ = Print.printLn "done."
