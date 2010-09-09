val tens = [| n + 2 | n in [| 8 |] |]

(* FIXME This program exposed the fact that downward ranges don't work!!! *)
val nums = [| m + n | m in [| 1 to 9 |], n in [| 11 to 19 |] |] 

val _ = Print.printLn (PArray.toString Int.toString "," tens)
val _ = Print.printLn (PArray.toString Int.toString "," nums)

val _ = Print.printLn "Done."
