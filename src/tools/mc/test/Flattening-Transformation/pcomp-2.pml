val vec = [| 1 to 10 |]
val square = [| [| 1 to 10 |] | r in vec |]

val _ = Print.printLn "done."
