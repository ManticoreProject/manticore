(*
val elt = [| 10, 11, 12, 13, 14, 15, 16, 17, 18 |]!0
val _ = Print.printLn ("expecting 10: " ^ Int.toString elt)
*)

val elt = [| [| 1, 2 |], [| 3 |] |] ! 0 
val _ = Print.printLn ("expecting 2: " ^ Int.toString (PArray.length elt))

val _ = Print.printLn "done."
