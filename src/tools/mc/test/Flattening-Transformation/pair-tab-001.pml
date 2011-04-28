val arr = [| (n, n+1) | n in [| 1 to 10000 |] |]

fun add ((a,b),(c,d)) = (a+c,b+d)

val sum = PArray.reduce add (0,0) arr

val _ = let
  val (m, n) = sum
  in
    Print.printLn ("expecting 1,2: " ^ Int.toString m ^ "," ^ Int.toString n)
  end

val _ = Print.printLn "done."
