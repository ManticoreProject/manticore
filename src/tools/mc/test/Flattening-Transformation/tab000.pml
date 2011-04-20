val arr = PArray.tabFromToStep (0, 999, 1, fn n => n)
val _ = Print.printLn ("expecting 1000: " ^ Int.toString (PArray.length arr))
val _ = Print.printLn "done."
