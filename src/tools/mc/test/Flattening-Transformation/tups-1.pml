(* stress testing FT tuple unzipping *)

val arr = [| (n, (n+1, (n+2, n+3), n+4, n+5)) | n in [| 1 to 10 |] |]

val foo = (1, (2, (3, 4), 5, 6))

fun tos (a, (b, (c, d), e, f)) = let
  val itos = Int.toString
  in
    String.concat ["(", itos a, ",(", itos b, ",(", itos c, ",", itos d, "),",
		   itos e, ",", itos f, "))"]
  end

fun pr x = Print.printLn (tos x)

val _ = Print.printLn "expecting (1,(2,(3,4),5,6)):"
val _ = pr (arr!0)

val _ = Print.printLn "expecting (2,(3,(4,5),6,7)):"
val _ = pr (arr!1) 

val _ = Print.printLn "expecting (10,(11,(12,13),14,15)):"
val _ = pr (arr!9) 

val _ = Print.printLn "done."
