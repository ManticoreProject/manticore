fun add (a, b) = a+b
fun mul (a, b) = a*b

fun mkPair n = (Float.fromInt(n) * 0.0000000001, Float.fromInt(n) * 0.0000001)

val arr = [| mkPair n | n in [| 0 to 9999999 |] |]

val dotp = PArray.reduce add 0.0 (PArray.map mul arr)

val _ = Print.printLn ("expecting 3333.33 or so: " ^ Float.toString dotp)

val _ = Print.printLn "done."
