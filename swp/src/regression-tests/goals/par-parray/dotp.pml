(* cheating...these names are supposed to be bound already... *)

fun sumP_float a = let
  fun add (x, y) = x + y
  in
    PArray.reduce (add, 0.0, a)
  end

fun sumP_int a = let
  fun add (x, y) = x+y
  in
    PArray.reduce (add, 0, a)
  end

val sumP = sumP_int

val sub = PArray.sub

fun lenP a = PArray.length a

(* real stuff *)

val itos = Int.toString
val ftos = Float.toString

(*
type vector = float parray
type sparse_vector = (int * float) parray
type sparse_matrix = sparse_vector parray
*)

val vtos = PArray.toString Int.toString ","

fun pointwiseMul (v1, v2) = [| x * y | x in v1, y in v2 |]

val _ = Print.printLn (vtos [| 1 to 10 |])

val x = pointwiseMul ([| 1 to 10 |], [| 2, 2, 2 |])
val _ = Print.printLn (vtos x)

val y = pointwiseMul ([| 1 to 10 |], [| 2 | n in [| 1 to 10 |] |])
val _ = Print.printLn (vtos y)

fun dotp (ms, ns) = sumP (pointwiseMul (ms, ns))
val dp = dotp ([| 1 to 100 |], [| 1 | _ in [| 1 to 100 |] |])
val _ = Print.printLn ("expecting 5050 => " ^ Int.toString dp) 

val _ = Print.printLn "Done."

