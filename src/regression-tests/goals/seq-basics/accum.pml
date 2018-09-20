fun theFunction x unused =
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
