fun signum (n : int) =
    if (n > 0) then 1 
    else if (n < 0) then ~1
    else 0

fun f () = let 
      val (x,y) = (| signum 1000, signum (~1000) |) 
      in
         Print.printLn (Int.toString x ^ " " ^ Int.toString y)
      end

val cilk5 = MultiprogrammedWorkStealing.workGroup()
val () = ImplicitThread.runWithGroup(cilk5, f)
