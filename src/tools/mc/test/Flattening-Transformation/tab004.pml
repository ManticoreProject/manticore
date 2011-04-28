val a = PArray.tab (10, fn n => n)

val _ = Print.printLn (Int.toString (a!0))
val _ = Print.printLn (Int.toString (a!1))
val _ = Print.printLn (Int.toString (a!2))
val _ = Print.printLn (Int.toString (a!9))

val _ = Print.printLn "done"
