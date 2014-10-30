(* linked-list.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Linked list implementation based on Software Transactional Memory with partial aborts.
 *)

structure WhichSTM = FullAbortSTM

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

fun add (l:ListHandle) (v:int) = 
    let fun lp l = 
            case get l 
                of Head n => lp n
                 | Null => put(l, Node(v, new Null))
                 | Node(v', n) => 
                    if v' > v
                    then put(l, Node(v, new (Node(v', n))))
                    else lp n 
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
                 | Node(v', n) => if v = v' then true else lp n
    in atomic (fn () => lp l) end

fun next l = 
    case l 
        of Head n => n
         | Node(_, n) => n

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
            end
    in atomic(fn () => lp l) end            

val ITERS = 1200
val THREADS = 4
val MAXVAL = 100000

fun ignore _ = ()

fun threadLoop l i = 
    if i = 0
    then ()
    else let val randNum = Rand.inRangeInt(0, MAXVAL)
             val prob = Rand.inRangeInt(0, 7)
             val _ = case prob
                        of 0 => ignore(delete l randNum)
                         | 1 => ignore(find l randNum)
                         | 2 => ignore(find l randNum )
                         | _ => ignore(add l randNum)
         in threadLoop l (i-1) end
         
fun start l i =
    if i = 0
    then nil
    else let val ch = PrimChan.new()
             val _ = spawn(threadLoop l ITERS; PrimChan.send(ch, i))
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

val _ = initialize 2000
val startTime = Time.now()
val _ = join(start l THREADS)
val endTime = Time.now()
val _ = printStats()
val _ = print ("Total was: " ^ Time.toString (endTime - startTime) ^ " seconds\n")















