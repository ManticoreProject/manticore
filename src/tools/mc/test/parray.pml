(* using var names that are easy to chase down in debugging... *)

val LENNON = [| 0, 1, 2, 3 |]

val STARR = [| "a", "b", "c", "d" |]

val McCARTNEY = [| 0.1, 0.2, 0.3, 0.4, 0.5 |]

val HARRISON = [| 1 |]

(* 
val PETE_BEST = [| |]
*)

val _ = Print.printLn "expecting 4"
val _ = Print.printLn (Int.toString (PArray.length LENNON))

val _ = Print.printLn "expecting 4"
val _ = Print.printLn (Int.toString (PArray.length STARR))

val _ = Print.printLn "expecting 5"
val _ = Print.printLn (Int.toString (PArray.length McCARTNEY))

val _ = Print.printLn "expecting 1"
val _ = Print.printLn (Int.toString (PArray.length HARRISON))

(*
val _ = Print.printLn "expecting 0"
val _ = Print.printLn (Int.toString (PArray.length PETE_BEST))
*)

val _ = Print.printLn "done"
