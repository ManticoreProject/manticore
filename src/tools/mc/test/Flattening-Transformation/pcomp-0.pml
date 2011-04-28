val arr = [| x+1 | x in [| 1, 2, 3, 4 |] |]

fun go () = let
  fun lp i = 
    if i=4 then () else let
      val n = arr ! i
      in
       (Print.printLn ("expecting " ^ Int.toString (i+2) ^ ": " ^ Int.toString n);
        lp (i+1))
      end
  in
    lp 0
  end

val _ = go ()

val _ = Print.printLn "done."
