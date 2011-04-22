val arr = [| (1, 2), (3, 4) |]
fun do (i, j) = Print.printLn (Int.toString (i+j))
val _ = PArray.app do arr
val _ = Print.printLn "done."
