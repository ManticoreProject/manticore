val elt1 = [| 10, 11, 12, 13, 14, 15, 16, 17, 18 |] ! 0
val _ = Print.printLn ("expecting 10: " ^ Int.toString elt1)

val _ = Print.printLn "done."
