(* linked-list.pml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Linked list implementation based on Software Transactional Memory with partial aborts.
 * This was translated from the implementation given in:
 * "Comparing the performance of concurrent linked list implementations in Haskell"
 * http://dl.acm.org/citation.cfm?id=1481845&CFID=584191872&CFTOKEN=56043414
 *)





type 'a tvar = 'a STM.tvar
datatype List = Node of int * List tvar
              | Null
              | Head of List tvar

type ListHandle = (List tvar) tvar * (List tvar) tvar

fun getHead ((h, t) : ListHandle) = h
fun getTail ((h, t) : ListHandle) = t

fun newList (): ListHandle = 
    let val null = STM.new Null
        val hd = STM.new (Head null)
        val hdPtr = STM.new hd
        val tailPtr = STM.new null
    in (hdPtr, tailPtr) end

fun addToTail ((_, tailPtrPtr) : ListHandle) (v : int) : List tvar = 
    let fun trans() : List tvar = 
            let val null = STM.new Null
                val tailPtr = STM.get tailPtrPtr
                val _ = STM.put(tailPtr, Node(v, null))
                val _ = STM.put(tailPtrPtr, null)
            in tailPtr end
    in (STM.atomic trans) end

fun find (ptrPtr, _) i = 
    let fun find2 curNodePtr i = 
            case STM.get curNodePtr
                of Null => false
                 | Node(curval, curnext) =>
                    if curval = i then true else find2 curnext i
        fun trans() = 
            let val ptr = STM.get ptrPtr
                val Head startPtr = STM.get ptr
            in find2 startPtr i end
    in STM.atomic trans end

fun next l = case l 
                of Node(_, n) => n
                 | Head n => n

fun delete_curried(ptrPtr, _) i =
    let fun delete2 prevPtr i = 
            let val prevNode = STM.get prevPtr
                val curNodePtr = next prevNode
            in case STM.get curNodePtr
                of Null => false
                 | Node(curval, nextNode) => 
                    if curval <> i
                    then delete2 curNodePtr i
                    else (case prevNode
                            of Head _ => (STM.put(prevPtr, Head nextNode); true)
                             | Node(v, _) => (STM.put(prevPtr, Node(v, nextNode)); true))
            end
        fun trans() = delete2 (STM.get ptrPtr) i
    in STM.atomic trans end

fun delete(ptrPtr, _) i =
    let fun delete2(prevPtr, i) = 
            let val prevNode = STM.get prevPtr
                val curNodePtr = next prevNode
            in case STM.get curNodePtr
                of Null => false
                 | Node(curval, nextNode) =>
                    if curval <> i
                    then delete2(curNodePtr, i)
                    else (case prevNode
                            of Head _ => (STM.put(prevPtr, Head nextNode); true)
                             | Node(v, _) => (STM.put(prevPtr, Node(v, nextNode)); true))
            end
        fun trans() = delete2(STM.get ptrPtr, i)
    in STM.atomic trans end

fun printList (ptrPtr, _) = 
    let fun lp curPtr = 
            case STM.get curPtr
                of Null => print "\n"
                 | Head n => lp n
                 | Node(v, n) => (print (Int.toString v ^ ", "); lp n)
    in lp (STM.get ptrPtr) end

val head = newList()

val _ = addToTail head 1
val _ = addToTail head 2
val _ = addToTail head 3
val _ = addToTail head 4
val _ = addToTail head 5
val _ = addToTail head 6
val _ = addToTail head 7
val _ = addToTail head 8
val _ = addToTail head 9
val _ = addToTail head 10

val _ = delete head 5
val _ = delete head 3
val _ = (STM.atomic (fn _ => printList head))

val head = newList()

val _ = addToTail head 1
val _ = addToTail head 2
val _ = addToTail head 3
val _ = addToTail head 4
val _ = addToTail head 5
val _ = addToTail head 6
val _ = addToTail head 7
val _ = addToTail head 8
val _ = addToTail head 9
val _ = addToTail head 10

val _ = delete_curried head 5
val _ = delete_curried head 3
val _ = (STM.atomic (fn _ => printList head))

























