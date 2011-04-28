(* this program is written to test that flattening works inside datatypes *)

datatype dt
  = Int of int
  | Parr1 of int parray
  | Parr2 of int parray parray

fun f (Int n) = n
  | f (Parr1 ns) = if PArray.length(ns) = 0 then 1 else (ns ! 0)
  | f (Parr2 nss) = if PArray.length(nss) = 0 then 2 else f (Parr1 (nss ! 0))

val arr1 = [| 7, 8, 9 |]
val arr2 = [| arr1 |]

val _ = Print.printLn ("expecting 2: " ^ Int.toString (f (Int 2)))
val _ = Print.printLn ("expecting 7: " ^ Int.toString (f (Parr1 arr1)))
val _ = Print.printLn ("expecting 7: " ^ Int.toString (f (Parr2 arr2)))

val _ = Print.printLn "done."

