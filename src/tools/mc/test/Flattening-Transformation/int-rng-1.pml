val itos = Int.toString
val ln = Print.printLn
val compose = fn f => fn g => fn x => f (g x)

val pr = compose ln itos

val r = [| 1 to 10 by ~1|]
val _ = ln "the contents of r:"
val _ = PArray.app pr r

val _ = ln "done"
