fun sumP a = PArray.reduce (fn (a,b) => a+b) 0 a

val r1 = [| 0 to 100 |]
val _ = Print.printLn (PArray.tos_int r1)
val _ = Print.printLn (Int.toString (sumP r1))

val _ = Print.printLn "Done."

