fun add (a, b) = a+b
fun mul (a, b) = a*b
fun max (a, b) = if a>b then a else b
fun min (a, b) = if a>b then b else a

val arr = [| (1, 2), (3, 4) |]
val x = PArray.reduce add 0 (PArray.map mul arr)

val arr2 = [| (n,n) | n in [| 1 to 1000000 |] |]
val y = PArray.reduce add 0 (PArray.map max arr2)
val z = PArray.reduce add 0 (PArray.map min arr2)

val _ = Print.printLn ("expecting 14: " ^ Int.toString x)
val _ = Print.printLn ("expecting ??: " ^ Int.toString y)
val _ = Print.printLn "done."
