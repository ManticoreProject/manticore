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

val ITERS = getIntFlg "-iters" 10000

val tv = STM.new 0

fun lp i = 
    if i = 0
    then ()
    else (STM.atomic(fn () => (STM.put(tv, STM.get tv + 1);STM.put(tv, STM.get tv + 1))); lp (i-1))

fun start i =
    if i = 0
    then nil
    else let val ch = PrimChan.new()
             val _ = Threads.spawnOn(i-1, fn _ => PrimChan.send(ch, lp ITERS))
         in ch::start (i-1) end         

fun join chs = 
    case chs
        of ch::chs' => 
            let val _ = PrimChan.recv ch 
            in join chs' end
         | nil => nil



val _ = join (start (VProc.numVProcs()))

val _ = if (STM.unsafeGet tv = VProc.numVProcs() * ITERS * 2) 
        then print "Correct\n"
        else print("Incorrect, should be " ^ Int.toString (VProc.numVProcs() * ITERS * 2) ^ ", but got " ^ Int.toString (STM.unsafeGet tv) ^ "\n")

val _ = STM.printStats()







        
