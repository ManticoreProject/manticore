fun pr (a, b) = Print.printLn ("(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")")
val tups = [| (1, 2), (3, 4) |]

val elt0 = tups ! 0
val _ = Print.printLn "expecting (1,2):"
val _ = pr elt0

val elt1 = tups ! 1
val _ = Print.printLn "expecting (3,4):"
val _ = pr elt1

val _ = Print.printLn "done."
