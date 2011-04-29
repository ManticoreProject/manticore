val ln = Print.printLn
val cat = String.concat
val itos = Int.toString

val arr = [| (n, n+1) | n in [| 1 to 10000 |] |]

fun add ((a,b),(c,d)) = (a+c,b+d)

val sum = PArray.reduce add (0,0) arr

val _ = let
  val (m, n) = sum
  in
    ln (cat ["expecting 50005000,50015000: ", itos m, ",", itos n])
  end

val _ = Print.printLn "done."
