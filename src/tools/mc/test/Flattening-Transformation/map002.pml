fun pr (a, b) = Print.printLn ("(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")")

fun swap (a, b) = (b, a)
val arr = [| (1, 2) |]
val arr' = PArray.map swap arr

val _ = Print.printLn "expecting (2,1):"
val _ = pr (arr' ! 0)

val _ = Print.printLn "done."
