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

val tv1 = STM.new 0
val tv2 = STM.new 0

fun lp i = 
    if i = 0
    then ()
    else 
	let val _ = STM.atomic(fn () => let 
				   val _ = STM.put(tv1, STM.get tv1 + 1)
				   val _ = STM.put(tv1, STM.get tv1 + 1)
				   val _ = STM.put(tv2, STM.get tv2 + 1) in
				   STM.put(tv2, STM.get tv2 + 1) end)
	in lp (i-1)end

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

val v1 = STM.unsafeGet tv1
val v2 = STM.unsafeGet tv2
val correct = VProc.numVProcs() * ITERS * 2

val _ = if v1 = correct andalso v2 = correct
        then print "Correct\n"
        else (print(String.concat["Incorrect: should be ", Int.toString correct, 
				", but got: ", Int.toString v1, " and ", Int.toString v2, "\n"]);
	      raise Fail "Incorrect")

val _ = STM.printStats()
