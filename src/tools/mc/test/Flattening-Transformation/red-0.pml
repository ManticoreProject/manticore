fun add (a:int, b:int) = a+b

val arr = [| 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 |]

val sum = PArray.reduce add 0 arr

val _ = Print.printLn ("expecting 55: " ^ Int.toString sum)

val _ = Print.printLn "done."
