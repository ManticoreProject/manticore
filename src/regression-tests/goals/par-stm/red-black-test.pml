(* linked-list.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Linked list implementation based on Software Transactional Memory with partial aborts.
 *)

fun getArg f args = 
    case args 
        of arg::arg'::args => 
            if String.same(f, arg) then SOME arg'
            else getArg f (arg'::args)
         |_ => NONE

val args = CommandLine.arguments ()

val ITERS = 1000
val THREADS = VProc.numVProcs()
val MAXVAL = 1000

fun intComp(x, y) = if x < y then LESS else if x > y then GREATER else EQUAL

fun insertLoop (t, i, rands) = 
    if i = 0
    then rands
    else let val coin = Rand.inRangeInt(0, 2)
             val v = Rand.inRangeInt(0, MAXVAL)
             val _ = RBTree.insert intComp v t
	 in insertLoop(t, i-1, v::rands)
	 end

fun deleteLoop (t, vs) = 
    case vs 
        of hd::tl => (RBTree.remove intComp hd t; deleteLoop(t, tl))
         | nil => ()
            
fun thread ch n l = 
    let val rands = (insertLoop(l, n, nil) handle e => (PrimChan.send(ch, ()); raise e))
        val _ = (deleteLoop(l, rands) handle e => (PrimChan.send(ch, ()); raise e))
        val _ = PrimChan.send(ch, ())
    in () end
         
fun start l i =
    if i = 0
    then nil
    else let val ch = PrimChan.new()
             val _ = Threads.spawnOn(i-1, fn _ => (thread ch ITERS l))
         in ch::start l (i-1) end         

fun join chs = 
    case chs
        of ch::chs' => 
            let val () = PrimChan.recv ch 
            in join chs' end
         | nil => nil

val l = RBTree.newTree()

fun remove l v = 
    case l 
        of hd::tl => if hd = v then tl else hd::remove tl v
         | nil => (print "Incorrect: could not find element to remove!\n"; raise Fail "test failed\n")

fun nextList l = 
    case l 
        of hd::tl => SOME(hd, tl)
         | nil => NONE

fun writeList l f next =
    let val stream = TextIO.openOut f
        fun lp l = 
            case next l
                of SOME(hd, tl) => (TextIO.output(stream, Int.toString hd ^ ", "); lp tl)
                 | NONE => TextIO.output(stream, "\n")
    in lp l end



val _ = print ("Executing with " ^ Int.toString THREADS ^ " threads\n")
val _ = join(start l THREADS)

val _ = if RBTree.isEmpty l then () else (print "Nonempty tree after test\n"; raise Fail "")

val _ = STM.printStats()










