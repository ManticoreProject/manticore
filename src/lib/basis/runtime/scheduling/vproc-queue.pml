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

    _primcode (

    (* vproc queue structure (we use Q_EMPTY to mark an empty queue); the C runtime system relies
     * on this representation, so be careful when making changes.
     *)
      typedef queue = [FLS.fls, PT.fiber, any];

    (* takes a queue element (the first three arguments), a queue (lst), and another queue (rest), and 
     * produces the following queue:
     *    (first three arguments) -> rev(lst) -> rev(rest)
     *)
      define @reverse-queue (fls : FLS.fls, k : PT.fiber, lst : queue, rest : queue / exh : PT.exh) : queue =
	fun revQueue (fls : FLS.fls, k : PT.fiber, lst : queue, acc : queue / exh : PT.exh) : queue =
	     let acc : queue = alloc(fls, k, acc)
             if Equal(lst, Q_EMPTY)
                then return(acc)
	     else
		 apply revQueue (SELECT(FLS_OFF, lst), SELECT(FIBER_OFF, lst), SELECT(LINK_OFF, lst), acc / exh)
	let qitem : queue = apply revQueue (fls, k, lst, rest / exh)
	return (qitem)
      ;

      extern void *VProcDequeue (void *) __attribute__((alloc));

   (* dequeue from the secondary list *)
      define @dequeue-slow-path (vp : vproc / exh : PT.exh) : Option.option =
	let tl : queue = vpload (VP_RDYQ_TL, vp)
	if Equal(tl, Q_EMPTY)
	   then
	      return(NONE)
	else
	     do vpstore (VP_RDYQ_TL, vp, Q_EMPTY)
	     let qitem : queue = 
		   @reverse-queue (SELECT(FLS_OFF, tl), 
				   SELECT(FIBER_OFF, tl), 
				   (queue)SELECT(LINK_OFF, tl), 
				   Q_EMPTY 
				   / exh)
	     let link : queue = (queue)SELECT(LINK_OFF, tl)
	     do vpstore (VP_RDYQ_HD, vp, link)
	     return (Option.SOME(qitem))
      ;	  

    (* take from the local queue *)
      define @dequeue ( / exh : PT.exh) : Option.option =
	let vp : vproc = host_vproc
	let hd : queue = vpload (VP_RDYQ_HD, vp)
	if Equal(hd, Q_EMPTY)
	   then
	   (* the primary list is empty, so try the secondary list *)
	    let item : Option.option = @dequeue-slow-path (vp / exh)
	    return (item)
	else 
	    (* got a thread from the primary list *)
	    do vpstore (VP_RDYQ_HD, vp, #2(hd))
	    return (Option.SOME(hd))  
      ;

    (* enqueue on the vproc's thread queue *)
      define @enqueue (fls : FLS.fls, fiber : PT.fiber / exh : PT.exh) : () =
         let vp : vproc = host_vproc
	 let tl : queue = vpload (VP_RDYQ_TL, vp)
	 let qitem : queue = alloc(fls, fiber, tl)
	 do vpstore (VP_RDYQ_TL, vp, qitem)
	 return () 
      ;

    (* TODO: move this function to BOM *)
      extern void EnqueueOnVProc (void *, void *, void *, void *) __attribute__((alloc));

    (* unload threads from the landing pad *)
      define @unload-landing-pad (/ exh : PT.exh) : queue =
	let vp : vproc = host_vproc
	let mask : PT.bool = vpload(ATOMIC, vp)
	do vpstore(ATOMIC, vp, PT.TRUE)
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
	apply lp(queue, List.NIL / exh)
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
