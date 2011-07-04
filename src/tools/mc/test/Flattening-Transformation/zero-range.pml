(* FIXME examine the behavior of this program! *)

val arr = [| i+1 | i in [| 0 to ~100 |] |]

val _ = Print.printLn (PArray.tos_int arr)

val _ = Print.printLn (PArray.tos_int [| 0 to ~100 |])

val _ = Print.printLn "done"
