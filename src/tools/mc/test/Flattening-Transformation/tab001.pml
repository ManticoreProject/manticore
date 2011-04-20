val arr = PArray.tabFromToStep (0, 9, 1, fn n => (n, n+1))
val _ = Print.printLn ("expecting 10: " ^ Int.toString (PArray.length arr))
val _ = Print.printLn "done."

