
val new = FullAbortSTM.new
val put = FullAbortSTM.put
val get = FullAbortSTM.get
val atomic = FullAbortSTM.atomic
val printStats = FullAbortSTM.printStats

val tv = new 0

fun bump () =
    let val old = get tv
        val _ = put(tv, old+1)
    in () end

fun bumpN n =
    if n = 0
    then ()
    else (atomic bump; bumpN (n-1))

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

val res = atomic(fn () => get tv)
val _ = print ("Result is " ^ (Int.toString res) ^ "\n")
val _ = printStats()
