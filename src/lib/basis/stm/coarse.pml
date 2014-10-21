val tv = STM.new 0

fun fib n = 
    if n <= 1
    then 1
    else fib (n-1) + fib (n-2)

fun trans n = 
    if n = 0
    then ()
    else let val x = STM.get tv
             val _ = STM.put(tv, x+1)
         in trans (n-1) end
         
val ch1 = PrimChan.new()
val ch2 = PrimChan.new()

val n = 100

fun start n k = 
    if n = 0 
    then nil
    else let val ch = PrimChan.new()
             val _ = spawn (STM.atomic(fn _ => trans k); PrimChan.send(ch, STM.getID()))
         in ch::start (n-1) k
         end

fun join chs = 
    case chs 
        of ch::chs' => (PrimChan.recv ch; join chs')
         | nil => ()

val chs = start 10 n
val _ = join chs

val x = STM.get tv
val _ = print ("Result is " ^ Int.toString x ^ "\n")
