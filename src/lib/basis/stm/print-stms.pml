


fun printSTMs stms = 
    case stms
     of (stm, _)::stms => (print (stm ^ "\n"); printSTMs stms)
     | nil => ()

val _ = printSTMs (Ref.get (STMs.stms))


