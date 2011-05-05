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

val x = [| 1+1 to 2+2 by 7-6 |]
val y = [| 3+3 to 4+4 by 1*1*1 |]
val z = x
val a = y

val _ = pr "x" x
val _ = pr "y" y
val _ = pr "z" z
val _ = pr "a" a

val _ = Print.printLn "done"

