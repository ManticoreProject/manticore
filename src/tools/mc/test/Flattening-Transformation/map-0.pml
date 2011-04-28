fun incr n = n+1
val arr = [| 6 |]
val arr' = PArray.map incr arr
val _ = Print.printLn ("expecting 7: " ^ Int.toString (arr' ! 0))

val _ = Print.printLn "done."
