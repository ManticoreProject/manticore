(* vproc-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module contains functions for VProc thread queues. Each VProc owns a single 
 * queue, and access to the queue is restricted to certain patterns. A VProc can
 * only dequeue from its own queue, but can enqueue on either its own queue or a
 * remote queue.
 *
 * Because of these restricted operations, we can reduce the synchronization overhead
 * on queues. To do so, we use the principle of separation to design our queue structure.
 * VProc queues consist of three linked lists: the primary and secondary lists and the
 * landing-pad list. We can dequeue locally from the primary list, enqueue locally from 
 * the secondary list, and enqueue remotely from the landing-pad list.
 *)

#include "vproc-queue.def"

structure VProcQueue =
  struct

    structure FLS = FiberLocalStorage
    structure PT = PrimTypes
    structure O = Option

    _primcode (

    (* vproc queue structure (we use Q_EMPTY to mark an empty queue); the C runtime system relies
     * on this representation, so be careful when making changes.
     *)
      typedef queue = [FLS.fls, PT.fiber, any];

      define @is-queue-geq-one (q : queue / exh : PT.exh) : PT.bool =
	return(Equal(q, Q_EMPTY))
      ;

      define @is-local-queue-geq-one ( / exh : PT.exh) : PT.bool =
	let tl : queue = vpload (VP_RDYQ_TL, host_vproc)
	let hd : queue = vpload (VP_RDYQ_HD, host_vproc)
        let b : PT.bool = @is-queue-geq-one(tl / exh)
        if b
	   then return(b)
	else 
            @is-queue-geq-one(hd / exh)
      ;

      define @is-queue-gt-one (q : queue / exh : PT.exh) : int =
	if Equal(q, Q_EMPTY)
	   then return(0)
	else 
	    if Equal(SELECT(LINK_OFF, q), Q_EMPTY)
	       then return(1)
	    else return(2)
      ;

      define @is-local-queue-gt-one ( / exh : PT.exh) : PT.bool =
	let tl : queue = vpload (VP_RDYQ_TL, host_vproc)
	let hd : queue = vpload (VP_RDYQ_HD, host_vproc)
        let ntl : int = @is-queue-gt-one(tl / exh)
        let nhd : int = @is-queue-gt-one(hd / exh)
        return(I32Gt(I32Add(ntl, nhd), 1))
      ;

    (* takes a queue element (queue0) and a queue (rest), and produces the queue
     *    queue0 -> rev(rest)
     *)
      define @reverse-queue (fls : FLS.fls, k : PT.fiber, rest : queue / exh : PT.exh) : queue =
	fun revQueue (fls : FLS.fls, k : PT.fiber, rest : queue, acc : queue / exh : PT.exh) : queue =
	     let acc : queue = alloc(fls, k, acc)
             if Equal(rest, Q_EMPTY)
                then return(acc)
	     else
		 apply revQueue (SELECT(FLS_OFF, rest), SELECT(FIBER_OFF, rest), SELECT(LINK_OFF, rest), acc / exh)
	let qitem : queue = apply revQueue (fls, k, rest, Q_EMPTY / exh)
	return (qitem)
      ;

   (* dequeue from the secondary list *)
      define @dequeue-slow-path (vp : vproc / exh : PT.exh) : O.option =
	let tl : queue = vpload (VP_RDYQ_TL, vp)
	if Equal(tl, Q_EMPTY)
	   then
	      return(O.NONE)
	else
	     do vpstore (VP_RDYQ_TL, vp, Q_EMPTY)
	     let qitem : queue = 
		   @reverse-queue (SELECT(FLS_OFF, tl), 
				   SELECT(FIBER_OFF, tl), 
				   (queue)SELECT(LINK_OFF, tl)
				   / exh)
	     do vpstore (VP_RDYQ_HD, vp, (queue)SELECT(LINK_OFF, qitem))
	     return (O.SOME(qitem))
      ;	  

    (* take from the local queue *)
      define @dequeue ( / exh : PT.exh) : O.option =
	let vp : vproc = host_vproc
	let hd : queue = vpload (VP_RDYQ_HD, vp)
	if Equal(hd, Q_EMPTY)
	   then
	   (* the primary list is empty, so try the secondary list *)
	    @dequeue-slow-path (vp / exh)
	else 
	    (* got a thread from the primary list *)
	    do vpstore (VP_RDYQ_HD, vp, SELECT(LINK_OFF, hd))
	    return (O.SOME(hd))  
      ;

    (* Enqueue a thread in the host-vprocs ready queue.  This function
     * assumes that signals are masked.  Use @atomic-enqueue when signals
     * are not masked.
     *)
      define @enqueue (fls : FLS.fls, fiber : PT.fiber / exh : PT.exh) : () =
         let vp : vproc = host_vproc
	 let tl : queue = vpload (VP_RDYQ_TL, vp)
	 let qitem : queue = alloc(fls, fiber, tl)
	 do vpstore (VP_RDYQ_TL, vp, qitem)
	 return () 
      ;

    (* enqueue on the host's vproc's thread queue *)
      define inline @atomic-enqueue (fls : FLS.fls, fiber : PT.fiber / exh : PT.exh) : () =
	let vp : vproc = host_vproc
	do vpstore(ATOMIC, vp, PT.true)
	do @enqueue (fls, fiber / exh)
	do vpstore(ATOMIC, vp, PT.false)
	return ()
      ;

    (* dequeue the first item to satisfy f(SELECT(FLS_OFF, item)) *)
      define @dequeue-with-pred (f : fun(FLS.fls / PT.exh -> PT.bool) / exh : PT.exh) : O.option =
	let m : PT.bool = vpload(ATOMIC, host_vproc)
	do vpstore(ATOMIC, host_vproc, PT.true)

        cont exit (x : O.option) = 
    	  do vpstore(ATOMIC, host_vproc, m)
          return(x)

	let qitem : O.option = @dequeue(/ exh)
	case qitem
	 of O.NONE => throw exit(O.NONE)
	  | O.SOME (origItem : queue) =>
	    fun lp () : O.option =
		let qitem : O.option = @dequeue(/ exh)
		case qitem
		 of O.NONE => throw exit(O.NONE)
		  | O.SOME(item : queue) =>
		    let b : PT.bool = apply f (SELECT(FLS_OFF, item) / exh)
		    if b
		       then throw exit (O.SOME(item))
		    else if Equal(SELECT(FLS_OFF, item), SELECT(FLS_OFF, origItem))
		       then 
			do @enqueue(SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item) / exh)
			throw exit (O.NONE)
		    else 
			do @enqueue(SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item) / exh)
			apply lp()
		end
	    let b : PT.bool = apply f (SELECT(FLS_OFF, origItem) / exh)
	    if b
	       then 
		throw exit(O.SOME(origItem))
	    else
		do @enqueue(SELECT(FLS_OFF, origItem), SELECT(FIBER_OFF, origItem) / exh)
		apply lp()
	end
      ;

    (* unload threads from the landing pad *)
      define @unload-landing-pad (/ exh : PT.exh) : queue =
	let vp : vproc = host_vproc
	let mask : PT.bool = vpload(ATOMIC, vp)
	do vpstore(ATOMIC, vp, PT.true)
	fun lp () : queue =
	    let item : queue = vpload(VP_ENTRYQ, vp)
	    let queue : queue = CAS(&VP_ENTRYQ(vp), item, Q_EMPTY)
	    if Equal(queue, item)
	       then return(queue)
	    else apply lp()
	let item : queue = 
		 (* quick check to avoid the CAS operation *)
   	           let item : queue = vpload(VP_ENTRYQ, vp)
		   if Equal(item, Q_EMPTY)
		      then return(Q_EMPTY)
		   else apply lp ()
	do vpstore(ATOMIC, vp, mask)
	return(item)
      ;

      define @is-messenger-thread (queue : queue / exh : PT.exh) : PT.bool =
	return(Equal(SELECT(FLS_OFF, queue), MESSENGER_FLS))
      ;

    (* this function performs two jobs:
     *  1. put ordinary threads on the local queue
     *  2. returns any messenger threads
     *)
      define @process-landing-pad (queue : queue / exh : PT.exh) : List.list =
	fun lp (queue : queue, messengerThds : List.list / exh : PT.exh) : List.list =
	    if Equal(queue, Q_EMPTY)
	       then return(messengerThds)
	    else 
		let isMessenger : PT.bool = @is-messenger-thread(queue / exh)
		if isMessenger
		   then 
		  (* the head of the queue is a messenger thread *)
		    let messenger : List.list = List.CONS(SELECT(FIBER_OFF, queue), messengerThds)
		    apply lp((queue)SELECT(LINK_OFF, queue), messenger / exh)
		else
		  (* the head of the queue is an ordinary thread; put it on the local queue *)
		    do @enqueue (SELECT(FLS_OFF, queue), SELECT(FIBER_OFF, queue) / exh)
		    apply lp((queue)SELECT(LINK_OFF, queue), messengerThds / exh)
	apply lp(queue, nil / exh)
      ;

    (* unload the landing pad, and return any messages *)
      define @unload-and-check-messages (/ exh : PT.exh) : List.list =
	let queue : queue = @unload-landing-pad ( / exh)
	@process-landing-pad(queue / exh)
      ;

      extern void WakeVProc(void *);

      define @enqueue-on-vproc (dst : vproc, fls : FLS.fls, k : PT.fiber / exh : PT.exh) : () =
        fun lp () : queue =
	    let entryOld : queue = vpload(VP_ENTRYQ, dst)
            let entryNew : queue = alloc(fls, k, entryOld)
            let entryNew : queue = promote(entryNew)
            let x : queue = CAS(&VP_ENTRYQ(dst), entryOld, entryNew)
            if NotEqual(x, entryOld)
	       then apply lp()
	    else return(entryOld)
        let entryOld : queue = apply lp()

      (* wake the vproc if its queue was empty *)
        if Equal(entryOld, Q_EMPTY)
	   then do ccall WakeVProc(dst)
		return()
        else return()
      ;

    )

  end
