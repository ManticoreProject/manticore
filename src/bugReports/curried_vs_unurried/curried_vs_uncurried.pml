(**
 * I'm not really sure what the problem is here, but I occasionally have trouble with
 * nested curry functions not terminating when they should.  I've tried to come up
 * with the simplest example exhibiting the problem, and this is what I came up with.
 * The first implementation of "theFunction" terminates just fine, but, the the second
 * one that curries its nested functions diverges.
 * -Matt
*)
fun theFunction' x i = 
    let fun g(x, accum) = 
            case x
                of x::xs => h(xs, accum + x)
                 | nil => accum
        and h(x, accum) =
             case x
                of y::nil => g(x, accum)
                 | x::xs => h(xs, accum + x)
                 | nil => accum
    in g(x, 0) end

val _ = print("Result = " ^ Int.toString (theFunction' (List.tabulate(100, fn _ => 1)) 1000 ) ^ "\n")



fun theFunction x i = 
    let fun g x accum = 
            case x
                of x::xs => h xs (accum + x)
                 | nil => accum
        and h x accum =
             case x
                of y::nil => g x accum
                 | x::xs => h xs (accum + x)
                 | nil => accum
    in  g x 0 end

val _ = print("Result = " ^ Int.toString (theFunction (List.tabulate(100, fn _ => 1)) 1000 ) ^ "\n")


