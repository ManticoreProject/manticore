(* cheating...these names are supposed to be bound already... *)

(* real stuff *)

val itos = Int.toString
val ftos = Float.toString

(*
type vector = float parray
type sparse_vector = (int * float) parray
type sparse_matrix = sparse_vector parray
*)

fun plus (x, y) = x + y

fun sumP (id, xs) = reduceP (plus, id, xs)

fun dotp (sv, v) = sumP (0.0, [| x * (v!i) | (i,x) in sv |])

val sv0 = [| (0, 1.1), (5, 1.2) |]
val sv1 = [| (1, 1.3) |]
val v0  = [| 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 |]

val dotp0 = dotp (sv0, v0)

val _ = Print.printLn ("Testing dotp: expecting 2.3 => " ^ (ftos dotp0))

val dotp1 = let
  val rng = [| 0 to 999 |]
  in
    dotp ([| (n, 1.0) | n in rng |], [| 1.0 | n in rng |])
  end

val _ = Print.printLn ("Testing dotp: expecting 1000.0 => " ^ (ftos dotp1))

fun smvm (sm, v) = [| dotp (row, v) | row in sm |]

val sm0 = [| sv0, sv1 |]

val smvm0 = smvm (sm0, v0)

val _ = Print.printLn ("smvm0 => " ^ (PArray.toString ftos "," smvm0))

val _ = Print.printLn "Done."
