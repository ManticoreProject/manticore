val sz = 64
val field = [| [| (i,j) | i in [| 0 to sz-1 |] |] | j in [| 0 to sz-1 |] |]
val _ = Print.printLn (PArray.tos_intPair (field!0))
val _ = Print.printLn "done."
