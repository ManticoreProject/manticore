val elt2 = [| [| 1, 2 |], [| 3 |] |] ! 0 
val _ = Print.printLn ("expecting 2: " ^ Int.toString (PArray.length elt2))

val _ = Print.printLn "done."
