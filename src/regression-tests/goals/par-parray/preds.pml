val pos = [| n | n in [| ~100 to 10 |] where n > 0 |]
val _ = Print.printLn (PArray.toString Int.toString "," pos)

val neg = [| ~n | n in [| ~100 to 10 |] where n > 0 |]
val _ = Print.printLn (PArray.toString Int.toString "," neg)

val _ = Print.printLn "Done."
