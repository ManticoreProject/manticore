val arr = [| (n, (n+1, n+2)) | n in [| 1 to 10 |] |]

fun pr (a, (b, c)) = let
  val itos = Int.toString
  val s = String.concat ["(", itos a, ",(", itos b, ",", itos c, "))"]
  in
    Print.printLn s
  end

val _ = Print.printLn "expecting (1,(2,3)):"
val _ = pr (arr!0)

val _ = Print.printLn "expecting (2,(3,4)):"
val _ = pr (arr!1)

val _ = Print.printLn "done."
