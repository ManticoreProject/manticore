val arr = [| (1, 1.0), (2, 2.0), (3, 3.0) |]

fun pr (a, b) = let
  val itos = Int.toString
  val dtos = Double.toString
  val s = String.concat ["(", itos a, ",", dtos b, ")"]
  in
    Print.printLn s
  end

val _ = Print.printLn "expecting (1,1.0):"
val _ = pr (arr!0)

val _ = Print.printLn "expecting (2,2.0):"
val _ = pr (arr!1)

val _ = Print.printLn "expecting (3,3.0):"
val _ = pr (arr!2)

val _ = PArray.app pr arr

fun dotp (sm, v) = PArray.reduce (fn (x:double,y:double) => x+y) (0.0:double) 
		     (PArray.map (fn (i,x) => x * (v!i)) sm)

val res = dotp (arr, [| 1.0 | n in [| 1 to 20 |] |])

val _ = Print.printLn (Double.toString res)

val _ = Print.printLn "done."
