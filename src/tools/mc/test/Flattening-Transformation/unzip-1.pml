(* 
val a : (int * double) parray = [| (0, 1.2) |]
val _ = Print.printLn ("length of a: " ^ Int.toString (PArray.length a))
val _ = Print.printLn "********"
*)

val arr : (int * int) parray parray = [| [| (0, 1) |] |]
val _ = Print.printLn ("length of arr: " ^ Int.toString (PArray.length arr))

fun pairString (m,n) = let
  val itos = Int.toString
  in
    "(" ^ itos m ^ "," ^ itos n ^ ")"
  end

val _ = Print.printLn ("value is " ^ pairString (arr!0!0))

val _ = Print.printLn "done"
