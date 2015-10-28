fun getArg f args = 
    case args 
        of arg::arg'::args => 
            if String.same(f, arg) then SOME arg'
            else getArg f (arg'::args)
         |_ => NONE

val args = CommandLine.arguments ()

fun getIntFlg f dflt = 
    case getArg f args
        of SOME n => (case Int.fromString n of SOME n => n | NONE => dflt)
         | NONE => dflt

val TVARS = getIntFlg "-tvars" 100
val tvars = Vector.tabulate(TVARS, fn i => STM.new (i, 0))
val ITERS = getIntFlg "-iters" 10000
val SAMPLE = getIntFlg "-sample" 12
val WRITE = getIntFlg "-write" 10

fun sample i x = 
    if i = 0
    then x
    else sample (i-1) (STM.get (Vector.sub(tvars, Rand.inRangeInt(0, TVARS))))

fun write rands =
    case rands 
       of nil => ()
        | r::rs => 
            let val tv = Vector.sub(tvars, r)
                val (x, y) = STM.get tv
                val _ = STM.put(tv, (x, y+1))
            in write rs end

fun lp i = 
    if i = 0
    then nil
    else 
        let val rands = List.tabulate(WRITE, fn _ => Rand.inRangeInt(0, TVARS))
            val _ = STM.atomic(fn () => (sample SAMPLE (0, 0); write rands))
         in rands @ lp (i-1) end

fun start i =
    if i = 0
    then nil
    else let val ch = PrimChan.new()
             val _ = Threads.spawnOn(i-1, fn _ => PrimChan.send(ch, lp ITERS))
         in ch::start (i-1) end

fun join chs = 
    case chs
        of ch::chs' => 
            let val l = PrimChan.recv ch 
            in l @ join chs' end
         | nil => nil

val actual = Vector.tabulate(TVARS, fn _ => STM.new 0)

fun chk i = 
    if i = 0
    then 
        let val (_, x) = STM.unsafeGet (Vector.sub(tvars, i)) 
        in STM.unsafeGet (Vector.sub(actual, i)) = x end
    else let val x = STM.unsafeGet (Vector.sub(actual, i))
             val (_, y) = STM.unsafeGet (Vector.sub(tvars, i))
             val _ = if x = y then () else print("Incorrect: " ^ Int.toString i ^ ": should be " ^ Int.toString x ^ ", but found " ^ Int.toString y ^ "\n")
         in x = y andalso chk (i-1) end

fun populate hist = 
    case hist
        of x::xs => 
            let val tv = Vector.sub(actual, x)
                val _ = STM.unsafePut(tv, STM.unsafeGet tv + 1)
            in populate xs end
        | nil => chk(TVARS - 1)

val hist = join (start (VProc.numVProcs()))
val _ = if populate hist then print "Correct!\n" else (print "Incorrect: counts are wrong!\n"; STM.printStats(); raise Fail "Incorrect!\n") 

val _ = STM.printStats()






        
