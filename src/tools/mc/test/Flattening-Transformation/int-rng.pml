val itos = Int.toString
val ln = Print.printLn

fun pr i = ln ("loc" ^ itos i)

val r = [| 1 to 10 |]
val _ = ln ("expecting 1: " ^ itos (r!0))

val _ = ln "done"
