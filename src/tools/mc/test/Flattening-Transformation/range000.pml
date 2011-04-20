val len = 1000000
val arr = [| 1 to len |]
val _ = Print.printLn ("expecting " ^ Int.toString(len) ^ ": " ^ Int.toString (PArray.length arr))

val _ = Print.printLn "done."
