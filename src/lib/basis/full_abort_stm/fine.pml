(*Finer transactions*)
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
        of ch::chs' => (PrimChan.recv ch; join chs')
         | nil => ()
         
val _ = join (start 4 100)

val res = STM.atomic(fn () => STM.get tv)
val _ = print ("Result is " ^ (Int.toString res) ^ "\n")
val _ = STM.printCommits()
