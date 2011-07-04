val (itos, dtos) = (Int.toString, Double.toString)
fun pr (n, x) = Print.printLn ("(" ^ itos n ^ "," ^ dtos x ^ ")")

val arr : (int * double) parray = [| (0, 0.0), (1, 1.0) |]

val n = PArray.length arr
val _ = Print.printLn ("length of arr: " ^ Int.toString n)

val _ = pr (arr!0)
val _ = pr (arr!1)

fun f (n, x) = n+1
val arr' = PArray.map f arr

val _ = Print.printLn (itos (arr'!0))
val _ = Print.printLn (itos (arr'!1))

val _ = Print.printLn "done"
