val (itos, dtos) = (Int.toString, Double.toString)

val xs = [| 0.0:double, 0.1:double |]
val n = PArray.length xs

val _ = Print.printLn ("the length of xs is " ^ itos n)
val _ = Print.printLn ("the first elt of xs is " ^ dtos (xs!0))
val _ = Print.printLn ("the second elt of xs is " ^ dtos (xs!1))

fun add (x:double, y:double) = x+y
val _ = Print.printLn ("the sum of xs is " ^ dtos (PArray.reduce add 0.0 xs))

val _ = Print.printLn "done"
