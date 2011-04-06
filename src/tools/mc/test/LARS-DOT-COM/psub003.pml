(* note: breaks if compiled with cps flattening *)

val arr = [| [| 1, 2 |], [| 3 |] |]
val elt = arr ! 0 ! 0
val _ = Print.printLn ("expecting 1: " ^ Int.toString elt)

val _ = Print.printLn "done."
