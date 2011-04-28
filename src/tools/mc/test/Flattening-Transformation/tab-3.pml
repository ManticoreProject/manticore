val a = [| n*2 | n in [| 0 to 10 |] |]

val s = PArray.tos_int a

val _ = Print.printLn s
val _ = Print.printLn "done."

