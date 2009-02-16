(* vproc-queue.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module contains functions for VProc thread queues. Each VProc owns a single 
 * queue, and access to the queue is restricted to certain patterns. A VProc can
 * only dequeue from its own queue, but can enqueue on either its own queue or a
 * remote queue.
 *
 * VProc queues consist of three linked lists: the primary and secondary lists and the
 * landing-pad. We can dequeue locally on the primary list, enqueue locally on
 * the secondary list, and enqueue remotely on the landing-pad.
 *)

#include "vproc-queue.def"

structure VProcQueue (* :
  sig

    _prim(

    (* vproc queue structure (we use Q_EMPTY to mark an empty queue); the C runtime system relies
     * on this representation, so be careful when making changes.
     *)
      typedef queue = [FLS.fls, PT.fiber, any];

    (* enqueue on the local queue. NOTE: signals must be masked *)
      define @enqueue-in-atomic (vp : vproc, fls : FLS.fls, fiber : PT.fiber) : ();
    (* dequeue from the local queue  *)
      define @dequeue-in-atomic () : O.option;

    (* enqueue a fiber (paired with fls) on a remote vproc *)
      define @enqueue-on-vproc (dst : vproc, fls : FLS.fls, k : PT.fiber) : ();

    (* dequeue the first item to satisfy the given predicate  *)
      define @dequeue-with-pred (f : fun(FLS.fls / exh -> bool) / exh : exh) : O.option;
    (* enqueue on the host's vproc's thread queue *)
      define inline @enqueue (fls : FLS.fls, fiber : PT.fiber) : ();

    )

  end *) = struct

    structure PT = PrimTypes
    structure O = Option

    _primcode (

      extern void WakeVProc(void *);

    (* vproc queue structure (we use Q_EMPTY to mark an empty queue); the C runtime system relies
     * on this representation, so be careful when making changes.
     *)
      typedef queue = [FLS.fls, PT.fiber, any];

      define @is-queue-empty (q : queue) : bool =
	    return (NotEqual(q, Q_EMPTY))
	  ;

      define @is-local-queue-empty () : bool =
	  let tl : queue = vpload (VP_RDYQ_TL, host_vproc)
	  let hd : queue = vpload (VP_RDYQ_HD, host_vproc)
	  let b : bool = @is-queue-empty(tl)
	  if b
	     then return (b)
	  else 
	      @is-queue-empty(hd)
	;

      define @is-queue-gt-one (q : queue / exh : exh) : int =
	  if Equal(q, Q_EMPTY) then return  (0)
	  else if Equal(SELECT(LINK_OFF, q), Q_EMPTY) then return (1)
	  else return( 2)
	;

      define @is-local-queue-gt-one ( / exh : exh) : bool =
	  let tl : queue = vpload (VP_RDYQ_TL, host_vproc)
	  let hd : queue = vpload (VP_RDYQ_HD, host_vproc)
	  let ntl : int = @is-queue-gt-one(tl / exh)
	  let nhd : int = @is-queue-gt-one(hd / exh)
	  return(I32Gt(I32Add(ntl, nhd), 1))
	;

    (* enqueue on the local queue. NOTE: signals must be masked *)
      define @enqueue-in-atomic (vp : vproc, fls : FLS.fls, fiber : PT.fiber) : () =
	 let tl : queue = vpload (VP_RDYQ_TL, vp)
	 let qitem : queue = alloc(fls, fiber, tl)
	 do vpstore (VP_RDYQ_TL, vp, qitem)
	 return () 
      ;

    (* unload threads from the landing pad *)
      define @unload-landing-pad () : queue =
(* ASSERT: signals are masked *)
	  let vp : vproc = host_vproc
	  fun lp () : queue =
	      let item : queue = vpload(VP_ENTRYQ, vp)
	      let queue : queue = CAS(&VP_ENTRYQ(vp), item, Q_EMPTY)
	      if Equal(queue, item)
		then return(queue)
	        else
		  do Pause()
		  apply lp()
	  let item : queue = 
	    (* quick check to avoid the CAS operation *)
	      let item : queue = vpload(VP_ENTRYQ, vp)
	      if Equal(item, Q_EMPTY) then return(Q_EMPTY)
	      else apply lp ()
	  return(item)
	;

      define @is-messenger-thread (queue : queue) : bool =
	return(Equal(SELECT(FLS_OFF, queue), MESSENGER_FLS))
      ;

    (* this function performs two jobs:
     *  1. put ordinary threads on the local queue
     *  2. returns any messenger threads
     *)
      define @process-landing-pad (queue : queue) : List.list =
	  fun lp (queue : queue, messengerThds : List.list) : List.list =
	      if Equal(queue, Q_EMPTY)
		 then return(messengerThds)
	      else 
		  let isMessenger : bool = @is-messenger-thread(queue)
		  if isMessenger
		     then 
		    (* the head of the queue is a messenger thread *)
		      let messengerThds : List.list = List.CONS(SELECT(FIBER_OFF, queue), messengerThds)
		      apply lp((queue)SELECT(LINK_OFF, queue), messengerThds)
		  else
		    (* the head of the queue is an ordinary thread; put it on the local queue *)
		      do @enqueue-in-atomic (host_vproc, SELECT(FLS_OFF, queue), SELECT(FIBER_OFF, queue))
		      apply lp((queue)SELECT(LINK_OFF, queue), messengerThds)
	  apply lp(queue, nil)
	;

    (* unload the landing pad, and return any messages *)
      define @unload-and-check-messages () : List.list =
	  let queue : queue = @unload-landing-pad ()
	  @process-landing-pad(queue)
	;

    (* takes a queue element (queue0) and a queue (rest), and produces the queue
     *    queue0 -> rev(rest)
     *)
      define @reverse-queue (fls : FLS.fls, k : PT.fiber, rest : queue) : queue =
	  fun revQueue (fls : FLS.fls, k : PT.fiber, rest : queue, acc : queue) : queue =
	       let acc : queue = alloc(fls, k, acc)
	       if Equal(rest, Q_EMPTY)
		 then return(acc)
	       else
		 apply revQueue (SELECT(FLS_OFF, rest), SELECT(FIBER_OFF, rest), SELECT(LINK_OFF, rest), acc)
	  let qitem : queue = apply revQueue (fls, k, rest, Q_EMPTY)
	  return (qitem)
	;

   (* dequeue from the secondary list *)
      define @dequeue-slow-path (vp : vproc) : O.option =
	  let tl : queue = vpload (VP_RDYQ_TL, vp)
	  if Equal(tl, Q_EMPTY)
	    then return(O.NONE)
	    else
	      do vpstore (VP_RDYQ_TL, vp, Q_EMPTY)
	      let qitem : queue = @reverse-queue (
		      SELECT(FLS_OFF, tl), 
		      SELECT(FIBER_OFF, tl), 
		      (queue)SELECT(LINK_OFF, tl))
	      do vpstore (VP_RDYQ_HD, vp, (queue)SELECT(LINK_OFF, qitem))
	      return (O.SOME(qitem))
	;	  

    (* dequeue from the local queue  *)
      define @dequeue-in-atomic (vp : vproc) : O.option =
(* NOTE: with software polling, we do not need to do this check! *)
	  let messages : List.list = @unload-and-check-messages()
	  let vp : vproc = host_vproc
	  let hd : queue = vpload (VP_RDYQ_HD, vp)
	  if Equal(hd, Q_EMPTY)
	     then
	     (* the primary list is empty, so try the secondary list *)
	      @dequeue-slow-path (vp)
	  else 
	      (* got a thread from the primary list *)
	      do vpstore (VP_RDYQ_HD, vp, SELECT(LINK_OFF, hd))
	      return (O.SOME(hd))  
	;

    (* enqueue on the host's vproc's thread queue *)
      define inline @enqueue (fls : FLS.fls, fiber : PT.fiber) : () =
	  let vp : vproc = SchedulerAction.@atomic-begin()
	  do @enqueue-in-atomic (host_vproc, fls, fiber)
	  do SchedulerAction.@atomic-end(vp)
	  return ()
	;

    (* dequeue the first item to satisfy the given predicate  *)
      define @dequeue-with-pred (f : fun(FLS.fls / exh -> bool) / exh : exh) : O.option =
	  let self : vproc = SchedulerAction.@atomic-begin ()
	  cont exit (x : O.option) = 
	      do SchedulerAction.@atomic-end (self)
	      return(x)
	  let qitem : O.option = @dequeue-in-atomic(self)
	  case qitem
	   of O.NONE => throw exit(O.NONE)
	    | O.SOME (origItem : queue) =>
	      fun lp () : O.option =
		  let qitem : O.option = @dequeue-in-atomic(self)
		  case qitem
		   of O.NONE => throw exit(O.NONE)
		    | O.SOME(item : queue) =>
		      let b : bool = apply f (SELECT(FLS_OFF, item) / exh)
		      if b then throw exit (O.SOME(item))
		      else if Equal(SELECT(FLS_OFF, item), SELECT(FLS_OFF, origItem))
			then 
			  do @enqueue(SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
			  throw exit (O.NONE)
			else 
			  do @enqueue(SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
			  apply lp()
		  end
	      let b : bool = apply f (SELECT(FLS_OFF, origItem) / exh)
	      if b then throw exit(O.SOME(origItem))
	      else
		do @enqueue(SELECT(FLS_OFF, origItem), SELECT(FIBER_OFF, origItem))
		apply lp()
	  end
	;

      define @enqueue-on-vproc (dst : vproc, fls : FLS.fls, k : PT.fiber) : () =
	  fun lp () : queue =
	      let entryOld : queue = vpload(VP_ENTRYQ, dst)
	      let entryNew : queue = alloc(fls, k, entryOld)
	      let entryNew : queue = promote(entryNew)
	      let x : queue = CAS(&VP_ENTRYQ(dst), entryOld, entryNew)
	      if NotEqual(x, entryOld)
		then
		  do Pause ()
		  apply lp()
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
