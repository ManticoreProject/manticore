val NIXON = [| 1.0, 1.1, 1.2, 1.3, 1.4 |]
val len = PArray.length NIXON
val _ = Print.printLn ("expecting 5: " ^ Int.toString len)

val WASHINGTON = [| [| 1 |], [| 2, 3 |] |]
val len = PArray.length WASHINGTON
val _ = Print.printLn ("expecting 2: " ^ Int.toString len)

val JEFFERSON = [| (1, 2), (3, 4) |]
val len = PArray.length JEFFERSON
val _ = Print.printLn ("expecting 2: " ^ Int.toString len)

val MONROE = [| WASHINGTON, WASHINGTON, WASHINGTON, WASHINGTON |]
val len = PArray.length MONROE
val _ = Print.printLn ("expecting 4: " ^ Int.toString len)

val ADAMS = [| [| (1, 2), (3, 4) |], [| (5, 6) |] |]
val len = PArray.length ADAMS
val _ = Print.printLn ("expecting 2: " ^ Int.toString len)

val VAN_BUREN = [| [| [| 1 |] |] |]
val len = PArray.length VAN_BUREN
val _ = Print.printLn ("expecting 1: " ^ Int.toString len)

(* TODO: parrs of datatypes *)
(* val JACKSON = [| [| SOME 1, NONE, SOME 2 |], [| NONE |] |] *)
(* val len = PArray.length JACKSON *)
(* val _ = Print.printLn ("expecting 2: " ^ Int.toString len) *)

(* also todo...empty parrays *)

val _ = Print.printLn "done"
