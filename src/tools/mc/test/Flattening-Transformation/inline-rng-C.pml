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

val r = [| 10-9 to 100 * 10 by 10+7 |]
val pc = [| n+1 | n in r |]

val _ = pr "r" r
val _ = pr "pc" pc

val _ = Print.printLn "done"
