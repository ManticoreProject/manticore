

structure WhichSTM = HybridPartialSTM

val tv = WhichSTM.new 1

val ITERS = 10

fun innerLoop n x = 
    if n = 0
    then x
    else innerLoop (n-1) (WhichSTM.get tv)

fun outterLoop n x = 
    if n = 0
    then x
    else let val res = WhichSTM.atomic(fn _ => innerLoop ITERS 0)
         in outterLoop (n-1) res
         end

val x = outterLoop 1000000 0




