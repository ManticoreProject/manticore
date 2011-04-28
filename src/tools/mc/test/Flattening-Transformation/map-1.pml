fun add (a, b) = a+b
val arr = [| (1, 2) |]
val arr' = PArray.map add arr
val _ = Print.printLn ("expecting 3: " ^ Int.toString (arr' ! 0))
val _ = Print.printLn "done."
