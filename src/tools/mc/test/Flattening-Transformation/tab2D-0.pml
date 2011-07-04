fun loc i = Print.printLn ("loc" ^ Int.toString i)

val rng = [| 1 to 3 |]
(* val _ = loc 1 *)

val arr = [| [| i*100 + j | j in rng |] | i in rng |]
(* val _ = loc 2 *)

val _ = Print.printLn (PArray.tos_intParr arr)
val _ = Print.printLn "done"
