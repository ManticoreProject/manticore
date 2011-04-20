fun add (a, b) = a+b
fun mul (a, b) = a*b
fun dotp vs = PArray.reduce add 0 (PArray.map mul vs)

val arr = [| (1,1), (2,2), (3,3) |]
val res = dotp arr

val _ = Print.printLn ("expecting 14: " ^ Int.toString res)
val _ = Print.printLn "done."
