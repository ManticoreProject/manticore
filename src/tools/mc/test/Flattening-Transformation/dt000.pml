(* this program is written to test that flattening works inside datatypes *)

datatype dt
  = Int of int
  | Parr of int parray

fun f (Int n) = n
  | f (Parr ns) = if PArray.length(ns) = 0 then 1 else (ns ! 0)

val arr = [| 7, 8, 9 |]

val _ = Print.printLn ("expecting 2: " ^ Int.toString (f (Int 2)))
val _ = Print.printLn ("expecting 7: " ^ Int.toString (f (Parr arr)))

val _ = Print.printLn "done."

