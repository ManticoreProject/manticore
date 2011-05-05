val itos = Int.toString

fun pr x rng = let
  val n = PArray.length rng
  val _ = Print.print (x ^ ": ")
  in
    if (n<=10) then
      Print.printLn (PArray.tos_int rng)
    else let
      val rLast = rng!(n-1)
      val s = "[|" ^ itos (rng!0) ^ "," ^ itos (rng!1) ^ 
	      ".." ^ itos rLast ^ "|]"
      in
        Print.printLn s
      end
  end

fun f (r : int parray) = PArray.length r

val x = [| 1 to 20 |]
val y = [| f x to 100 |]

val _ = pr "x" x
val _ = pr "y" y

val _ = Print.printLn "done"
