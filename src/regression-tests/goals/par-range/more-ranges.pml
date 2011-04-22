val itos = Int.toString

fun hmph (lo, hi) = let
  val r = [| lo to hi |]
  in
    Print.printLn (PArray.tos_int r) 
  end

val _ = hmph (0, 2)
val _ = hmph (1, 2)
val _ = hmph (10, 20)
val _ = hmph (0, 20)
val _ = hmph (~10, 2)

val _ = Print.printLn "Done."
