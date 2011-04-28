val len = 1000000
val arr = [| 1 to len |]
val arr' = PArray.map (fn n => n+1) arr

val _ = Print.printLn ("expecting 100: " ^ Int.toString (arr  ! 99))
val _ = Print.printLn ("expecting 101: " ^ Int.toString (arr' ! 99))

val _ = Print.printLn ("expecting 1000000: " ^ Int.toString (arr  ! 999999))
val _ = Print.printLn ("expecting 1000001: " ^ Int.toString (arr' ! 999999))

val _ = Print.printLn ("expecting " ^ Int.toString(len) ^ ": " ^ Int.toString (PArray.length arr))
val _ = Print.printLn ("expecting " ^ Int.toString(len) ^ ": " ^ Int.toString (PArray.length arr'))

val _ = Print.printLn "done."
