fun signum (n : int) =
    if (n > 0) then 1 
    else if (n < 0) then ~1
    else 0

val (x,y) = (| signum 1000, signum (~1000) |) 
val ()  = Print.printLn (Int.toString x ^ " " ^ Int.toString y)
