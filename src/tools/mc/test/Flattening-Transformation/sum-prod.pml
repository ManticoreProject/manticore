fun add (a, b) = a+b
fun mul (a, b) = a*b

val arr = [| (1, 2), (3, 4) |]
val x = PArray.reduce add 0 (PArray.map mul arr)

val _ = Print.printLn ("expecting 14: " ^ Int.toString x)
val _ = Print.printLn "done."
