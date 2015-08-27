(*structure STM = NoRecFF*)

(*
 * Potential bug:
 * When executing with "-stm ffnorec", I'm getting a seg fault when 
 * attempting a fast-forward and calling M_PolyEq.  It seems that 
 * we are getting a couple of forwarding pointers in the return 
 * continuation closure being passed into the read function.  
 *
 * Inside the "write" function, the return continuation closes over
 * the parameter "i", which at allocation time has a forwarding pointer
 * as a header.  You can try and reproduce this phenomonon by setting a 
 * conditional breakpoint at the code address where the allocation takes 
 * place, by doing:
 *     
 *    breakpoint set --address <addr> --condition '((((Word_t * )<$reg>)[-1]) & 7) == 0']
 *
 * where <$reg> is the register holding the pointer to the suspicious heap object
 *)

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
val SAMPLE = getIntFlg "-sample" 10
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
             val _ = if x = y then () else print(Int.toString i ^ ": should be " ^ Int.toString x ^ ", but found " ^ Int.toString y ^ "\n")
         in x = y andalso chk (i-1) end

fun populate hist = 
    case hist
        of x::xs => 
            let val tv = Vector.sub(actual, x)
                val _ = STM.unsafePut(tv, STM.unsafeGet tv + 1)
            in populate xs end
        | nil => chk(TVARS - 1)

val hist = join (start (VProc.numVProcs()))
val _ = if populate hist then print "Correct!\n" else (print "Incorrect!\n"; raise Fail "Incorrect!\n")

val _ = STM.printStats()







        
