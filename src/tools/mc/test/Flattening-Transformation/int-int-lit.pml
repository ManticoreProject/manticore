fun pr i = Print.printLn ("loc" ^ Int.toString i)

val arr = [| [| 1, 2, 3 |], [| 4, 5, 6 |] |]

val _ = Print.printLn ("expecting 3: " ^ Int.toString (PArray.length (arr!0)))
val _ = Print.printLn ("expecting 3: " ^ Int.toString (PArray.length (arr!1)))

val _ = Print.printLn ("expecting [|1,2,3|]: " ^ PArray.tos_int (arr!0))
val _ = Print.printLn ("expecting [|4,5,6|]: " ^ PArray.tos_int (arr!1))

val _ = Print.printLn "done."
