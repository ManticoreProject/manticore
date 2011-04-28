val arr = [| x+1 | x in [| 1 to 20 |] |]

fun go () = let
  val n = PArray.length arr
  fun lp i = 
    if i=n then () else let
      val n = arr ! i
      in
       (Print.printLn ("expecting " ^ Int.toString (i+2) ^ ": " ^ Int.toString n);
        lp (i+1))
      end
  in
    lp 0
  end

val _ = Print.printLn "starting...should see 2 through 21..."
val _ = go ()
val _ = Print.printLn "done."
