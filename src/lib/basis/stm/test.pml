(*

val tv = STM.new 0

fun bump () =
    let val old = STM.get tv
        val _ = STM.put(tv, old+1)
    in () end

fun bumpN n =
    if n = 0
    then ()
    else (STM.atomic bump; bumpN (n-1))

fun start n k = 
    if n = 0 
    then nil
    else let val ch = PrimChan.new()
             val _ = spawn (bumpN k; PrimChan.send(ch, n))
         in ch::start (n-1) k
         end

fun join chs = 
    case chs 
        of ch::chs' => (print ("Received val: " ^ Int.toString (PrimChan.recv ch) ^ "\n"); join chs')
         | nil => ()
         
val _ = join (start 2 100)

val res = STM.atomic(fn () => STM.get tv)
val _ = print ("Result is " ^ (Int.toString res) ^ "\n")


*)

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
             val _ = spawn (STM.atomic(fn _ => trans k); print (Int.toString(STM.getID()) ^ " finished in sequential position: " ^ Int.toString n ^ "\n"); PrimChan.send(ch, STM.getID()))
         in ch::start (n-1) k
         end

fun join chs = 
    case chs 
        of ch::chs' => (print ("Received on channel " ^ Int.toString(PrimChan.recv ch) ^ "\n"); join chs')
         | nil => ()

val chs = start 10 n
val _ = join chs

val x = STM.get tv
val _ = print ("Result is " ^ Int.toString x ^ "\n")


