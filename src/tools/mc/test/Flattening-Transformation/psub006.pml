val arr0 = [| 1, 2, 3 |]
val arr1 = [| arr0, arr0 |]
val arr2 = [| arr1, arr1 |]

val elt = arr2 ! 1 ! 1 ! 1

val _ = Print.printLn ("expecting 2: " ^ Int.toString elt)
val _ = Print.printLn "done."