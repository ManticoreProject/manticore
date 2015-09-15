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
val tvars = Vector.tabulate(TVARS, fn _ => STM.new 0)
val ITERS = getIntFlg "-iters" 10000
val SAMPLE = getIntFlg "-sample" 12
val WRITE = getIntFlg "-write" 10

fun sample i x = 
    if i = 0
    then x
    else sample (i-1) (STM.get (Vector.sub(tvars, Rand.inRangeInt(0, TVARS))))

fun write i =
    if i = 0
    then nil
    else let val randNum = Rand.inRangeInt(0, TVARS)
             val tv = Vector.sub(tvars, randNum)
             val _ = STM.put(tv, STM.get tv + 1)
         in randNum::write (i-1) end

fun lp i = 
    if i = 0
    then nil
    else let val writes = STM.atomic(fn () => (sample SAMPLE 0; write WRITE))
         in writes @ lp (i-1) end

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
    then STM.unsafeGet (Vector.sub(actual, i)) = STM.unsafeGet (Vector.sub(tvars, i))
    else let val x = STM.unsafeGet (Vector.sub(actual, i))
             val y = STM.unsafeGet (Vector.sub(tvars, i))
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
val _ = if populate hist then print "Correct!\n" else (print "Incorrect: corrects are wrong!\n"; raise Fail "Incorrect!\n")

val _ = STM.printStats()






        
