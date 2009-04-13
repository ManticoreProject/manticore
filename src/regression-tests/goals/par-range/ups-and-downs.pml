val a = [| [| 1 to 2 by 1 |],
           [| 1 to 2 by 2 |],
           [| 1 to 3 by 1 |],
           [| 1 to 3 by 2 |],
           [| 2 to 1 by ~1 |],
           [| 2 to 1 by ~2 |],
           [| 3 to 1 by ~1 |],
           [| 3 to 1 by ~2 |],
           [| 1 to 5 |],
	   [| 5 to 1 |],
           [| 5 to 1 by ~1 |] |]

val s = PArray.toString (PArray.toString Int.toString ",") "," a

val _ = Print.printLn s

val _ = Print.printLn "Done."
