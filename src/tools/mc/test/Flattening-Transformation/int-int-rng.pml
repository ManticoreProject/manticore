val itos = Int.toString
val ln = Print.printLn

fun pr i = ln ("loc" ^ itos i)

val _ = pr (~10)

val r = [| 1 to 10 |]

val _ = ln ("expecting 1: " ^ itos (r!0))

val _ = pr (~5)

val arr = [| [| 1 to 3 |], [| 4 to 6 |] |]

val _ = pr 0

val _ = ln ("expecting 3: " ^ itos (PArray.length (arr!0)))
val _ = ln ("expecting 3: " ^ itos (PArray.length (arr!1)))

val _ = pr 10

val _ = ln ("expecting [|1,2,3|]: " ^ PArray.tos_int (arr!0))
val _ = ln ("expecting [|4,5,6|]: " ^ PArray.tos_int (arr!1))

val _ = pr 20

val _ = ln "done."
