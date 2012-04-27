val a = [| 1, 2, 3, 4, 5 |];
val b = [| [| 1, 2 |], [|4, 5 |] |];

val _ = Print.print (Int.toString (PArray.length a))
val _ = Print.print (Int.toString (PArray.length b))
