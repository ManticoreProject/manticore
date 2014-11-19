(* linked-list.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Linked list implementation based on Software Transactional Memory with partial aborts.
 *)

structure WhichSTM = BoundedHybridPartialSTMLowMem

val put = WhichSTM.put
val get = WhichSTM.get
val new = WhichSTM.new
val atomic = WhichSTM.atomic
type 'a tvar = 'a WhichSTM.tvar
val printStats = WhichSTM.printStats

datatype List = Node of int * List tvar
              | Null
              | Head of List tvar

type ListHandle = List tvar

fun newList() : ListHandle = new (Head(new Null))

fun spin n = if n = 0 then 1 else spin (n-1)

fun getArg f args = 
    case args 
        of arg::arg'::args => 
            if String.same(f, arg) then SOME arg'
            else getArg f (arg'::args)
         |_ => NONE

val args = CommandLine.arguments ()

val c = case getArg "-spin" args
        of SOME n => (case Int.fromString n of SOME n => n | NONE => 0)
         | NONE => 0

val THREADS = 
    case getArg "-threads" args
        of SOME n => (case Int.fromString n of SOME n => n | NONE => 4)
         | NONE => 4


fun add (l:ListHandle) (v:int) = 
    let fun lp l = 
            case get l 
                of Head n => lp n
                 | Null => put(l, Node(v, new Null))
                 | Node(v', n) => 
                    if v' > v
                    then put(l, Node(v, new (Node(v', n))))
                    else (spin c; lp n)
    in atomic (fn () => lp l) end

fun printList (l:ListHandle) = 
    case get l
        of Null => print "\n"
         | Head n => printList n
         | Node(v, n) => (print (Int.toString v ^ ", "); printList n)

fun find (l:ListHandle) v = 
    let fun lp l = 
            case get l
                of Null => false
                 | Head n => lp n
                 | Node(v', n) => if v = v' then true else (spin c; lp n)
    in atomic (fn () => lp l) end

fun next l = 
    case l 
        of Head n => n
         | Node(_, n) => n
         | Null => raise Fail("trying to take next of null")

fun delete (l:ListHandle) (i:int) = 
    let fun lp prevPtr = 
            let val prevNode = get prevPtr
                val curNodePtr = next prevNode
            in case get curNodePtr
                    of Null => false
                     | Node(curVal, nextPtr) =>
                        if curVal = i
                        then (case prevNode
                                of Head _ => (put(prevPtr, Head nextPtr); true)
                                 | Node(v, _) => (put(prevPtr, Node(v, nextPtr)); true))
                        else lp curNodePtr
                     | Head n => raise Fail "found head node as next\n"
            end
    in atomic(fn () => lp l) end            

fun deleteIndex (l:ListHandle) (i:int) = 
    let fun lp(prevPtr, i) = 
            let val prevNode = get prevPtr
                val curNodePtr = next prevNode
            in case get curNodePtr
                    of Null => false
                     | Node(curVal, nextPtr) =>
                        if i = 0
                        then (case prevNode
                                of Head _ => (put(prevPtr, Head nextPtr); true)
                                 | Node(v, _) => (put(prevPtr, Node(v, nextPtr)); true))
                        else lp(curNodePtr, i-1)
                     | Head n => raise Fail "found head node as next\n"
            end
    in atomic(fn () => lp(l, i)) end            


val ITERS = 3000
val MAXVAL = 10000

val INITSIZE = 
    case getArg "-threads" args
        of SOME n => (case Int.fromString n of SOME n => n | NONE => 4000)
         | NONE => 4000
         
fun ignore _ = ()

val READS = 2
val WRITES = 4
val DELETES = 1

fun threadLoop l i = 
    if i = 0
    then ()
    else let val prob = Rand.inRangeInt(0, READS+WRITES+DELETES)
             val _ = if prob < READS
                     then ignore(find l (Rand.inRangeInt(0, MAXVAL)))
                     else if prob < READS + WRITES
                          then ignore(add l (Rand.inRangeInt(0, MAXVAL)))
                          else ignore(deleteIndex l (Rand.inRangeInt(0, INITSIZE)))
         in threadLoop l (i-1) end
         
fun start l i =
    if i = 0
    then nil
    else let val ch = PrimChan.new()
             val _ = Threads.spawnOn(i-1, fn _ => (threadLoop l ITERS; PrimChan.send(ch, i)))
         in ch::start l (i-1) end

fun join chs = 
    case chs
        of ch::chs' => (PrimChan.recv ch; join chs')
         | nil => ()

val l = newList()

fun initialize n = 
    if n = 0
    then ()
    else let val randNum = Rand.inRangeInt(0, MAXVAL)
             val _ = add l randNum
         in initialize (n-1) end

val _ = print ("Running with " ^ Int.toString THREADS ^ " threads\n")

val _ = initialize INITSIZE
val startTime = Time.now()
val _ = join(start l THREADS)
val endTime = Time.now()
val _ = printStats()
val _ = print ("Total was: " ^ Time.toString (endTime - startTime) ^ " seconds\n")















