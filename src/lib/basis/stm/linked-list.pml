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
            let val curNode = STM.get curNodePtr
            in case curNode
                of Null => false
                 | Node(curval, curnext) => 
                    if curval = i
                    then true
                    else find2 curnext i
            end
        fun trans() = 
            let val ptr = STM.get ptrPtr
                val Head startPtr = STM.get ptr
            in find2 startPtr i end
    in STM.atomic trans end

fun next l = case l 
                of Node(_, n) => n
                 | Head n => n

fun valOf l = case l 
                of Node(v, _) => v
                 
fun delete(ptrPtr, _) i =
    let fun delete2 prevPtr i = 
            let val prevNode = STM.get prevPtr
                val curNodePtr = next prevNode
                val curNode = STM.get curNodePtr
            in case curNode
                of Null => false
                 | Node(curval, nextNode) =>
                    if curval <> i
                    then delete2 curNodePtr i
                    else (case prevNode
                            of Head _ => (STM.put(prevPtr, Head nextNode); true)
                             | Node _ => (STM.put(prevPtr, Node(valOf prevNode, nextNode)); true))
            end
        fun trans() = 
            let val startPtr = STM.get ptrPtr
            in delete2 startPtr i end
    in STM.atomic trans end
                 
val head = newList()
val l = addToTail head 12

val b = find head 12

val _ = if b then print "true\n" else print "false\n"

val b = delete head 12

val _ = if b then print "true\n" else print "false\n"

val b = find head 12

val _ = if b then print "true\n" else print "false\n"



