val arr = [| [| (1, 2), (3, 4) |], [| (5, 6) |] |]

val elt = arr ! 0 ! 1

fun fst (a, _) = a

val _ = Print.printLn ("expecting 3: " ^ Int.toString (fst elt))

val _ = Print.printLn "done."
