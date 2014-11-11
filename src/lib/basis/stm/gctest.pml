

structure WhichSTM = FullAbortSTM

val tv = WhichSTM.new 1

fun lpASDF n x = 
    if n = 0
    then x
    else lpASDF (n-1) (WhichSTM.get tv)

val x : int = lpASDF 100000 0





