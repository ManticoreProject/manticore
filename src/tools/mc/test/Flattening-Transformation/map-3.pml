fun incr n = n+1

fun incr' ns = PArray.map incr ns

val arr = [| 1, 2, 3, 4 |]

val arr' = incr' arr

fun pr i = Print.printLn ("expecting " ^ Int.toString (i+2) ^ ": " ^ Int.toString (arr' ! i))

val _ = (pr 0; pr 1; pr 2; pr 3)

val _ = Print.printLn "done."
