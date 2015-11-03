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

datatype List = Node of int * List STM.tvar
              | Null
              | Head of List STM.tvar

type ListHandle = List STM.tvar

fun newList() : ListHandle = STM.new (Head(STM.new Null))

fun add (l:ListHandle) (v:int)  = 
    let fun lp l = 
            case STM.get l 
                of Head n => lp n
                 | Null => STM.put(l, Node(v, STM.new Null))
                 | Node(v', n) => 
                    if v' > v
                    then STM.put(l, Node(v, STM.new (Node(v', n))))
                    else lp n 
    in STM.atomic (fn () => lp l) end

fun printList (l:ListHandle) = 
    case STM.get l
        of Null => print "\n"
         | Head n => printList n
         | Node(v, n) => (print (Int.toString v ^ ", "); printList n)

fun find (l:ListHandle) v = 
    let fun lp l = 
            case STM.get l
                of Null => false
                 | Head n => lp n
                 | Node(v', n) => if v = v' then true else lp n
    in STM.atomic (fn () => lp l) end

fun next l = 
    case l 
        of Head n => n
         | Node(_, n) => n

fun delete (l:ListHandle) (i:int) = 
    let fun lp prevPtr = 
            let val prevNode = STM.get prevPtr
                val curNodePtr = next prevNode
            in case STM.get curNodePtr
                    of Null => false
                     | Node(curVal, nextPtr) =>
                        if curVal = i
                        then (case prevNode
                                of Head _ => (STM.put(prevPtr, Head nextPtr); true)
                                 | Node(v, _) => (STM.put(prevPtr, Node(v, nextPtr)); true))
                        else lp curNodePtr
            end
    in STM.atomic(fn () => lp l) end            

val ITERS = 1000
val THREADS = VProc.numVProcs()
val MAXVAL = 1000

fun insertLoop(l, i, removed, remaining) = 
    if i = 0
    then (removed, remaining)
    else let val coin = Rand.inRangeInt(0, 2)
             val v = Rand.inRangeInt(0, MAXVAL)
             val _ = add l v
	 in if coin = 1
	    then insertLoop(l, i-1, v::removed, remaining)
	    else insertLoop(l, i-1, removed, v::remaining)
	 end

fun deleteLoop (l:ListHandle) (vs:int list) = 
    case vs 
        of hd::tl => (delete l hd; deleteLoop l tl)
         | nil => ()
            
fun thread ch n l = 
    let val (remove, remaining) = insertLoop(l, n, nil, nil)
        val _ = deleteLoop l remove
        val _ = PrimChan.send(ch, remaining)
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
            let val l = PrimChan.recv ch 
            in l @ join chs' end
         | nil => nil

val l = newList()

fun remove l v = 
    case l 
        of hd::tl => if hd = v then tl else hd::remove tl v
         | nil => (print "Incorrect: could not find element to remove!\n"; raise Fail "test failed\n")

fun check (l:ListHandle) (remaining:int list) = 
    case STM.unsafeGet l
        of Null => (case remaining
                      of _::_ => (print "Incorrect: nonempty list after test!\n"; raise Fail "test failed\n")
                       | nil => print "List is correct\n")
         | Node(v, next) => check next (remove remaining v)
         | Head n => check n remaining

fun nextList l = 
    case l 
        of hd::tl => SOME(hd, tl)
         | nil => NONE

fun nextLinkedList l =
    case STM.get l
        of Null => NONE
         | Node(v, next) => SOME(v, next)
         | Head n => nextLinkedList n

fun writeList l f next =
    let val stream = TextIO.openOut f
        fun lp l = 
            case next l
                of SOME(hd, tl) => (TextIO.output(stream, Int.toString hd ^ ", "); lp tl)
                 | NONE => TextIO.output(stream, "\n")
    in lp l end


val remaining = join(start l THREADS)

val _ = print "Done with linked list operations\n"

val _ = check l remaining handle Fail s => print s

val _ = STM.printStats()










