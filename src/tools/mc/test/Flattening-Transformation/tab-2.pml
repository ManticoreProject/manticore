val arr = PArray.tab (10, fn n => PArray.tab (10, fn n => n))

val _ = Print.printLn ("expecting 10: " ^ Int.toString (PArray.length arr))
val _ = Print.printLn ("expecting 10: " ^ Int.toString (PArray.length (arr ! 0)))

val _ = Print.printLn "done."
