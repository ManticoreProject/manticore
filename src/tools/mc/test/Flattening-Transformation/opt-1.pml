val r = [| 1 to 100 |]
val SOME x = SOME [| n+n | n in r |]

val _ = Print.printLn ("expecting 100: " ^ Int.toString (PArray.length x))

val _ = Print.printLn "done"
