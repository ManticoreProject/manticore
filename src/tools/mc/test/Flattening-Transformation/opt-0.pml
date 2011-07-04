val SOME x = SOME [| n+n | n in [|1 to 100|] |]

val _ = Print.printLn ("expecting 100: " ^ Int.toString (PArray.length x))

val _ = Print.printLn "done"
