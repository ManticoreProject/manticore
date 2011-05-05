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

val a = [| 1 to 10 |]
val b = a
val c = b
val d = c
val e = d
val f = e

val _ = pr "a" a
val _ = pr "b" b
val _ = pr "c" c
val _ = pr "d" d
val _ = pr "e" e
val _ = pr "f" f

val _ = Print.printLn "done"
