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

val x = (Print.printLn "hello1"; 
	 Print.printLn "hello2";
	 Print.printLn "hello3";
	 [| 1 to 100 |])
val y = x

val _ = pr "x" x
val _ = pr "y" y

val _ = Print.printLn "done"