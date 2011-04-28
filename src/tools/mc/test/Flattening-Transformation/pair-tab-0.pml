val arr = [| (n, n+1) | n in [| 1 to 10000 |] |]

val _ = let
  val (m, n) = arr!0
  in
    Print.printLn ("expecting 1,2: " ^ Int.toString m ^ "," ^ Int.toString n)
  end

val _ = Print.printLn "done."
