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

structure VProcQueue (* :
  sig

    _prim(

    (* enqueue on the host's vproc's thread queue *)
      define inline @enqueue-from-atomic (vp : vproc, fls : FLS.fls, fiber : PT.fiber) : ();
      define inline @enqueue (fls : FLS.fls, fiber : PT.fiber) : ();

    (* dequeue from the local queue  *)
      define inline @dequeue-from-atomic () : O.option;

    (* dequeue the first item to satisfy the given predicate  *)
      define @dequeue-with-pred-from-atomic (f : fun(FLS.fls / exh -> bool) / exh : exh) : O.option;

    (* returns true if the local queue is empty *)
      define @is-local-queue-empty-from-atomic (self : vproc) : bool;

    (* returns true if the local queue contains more than one thread *)
      define @more-than-one-from-atomic (vp : vproc) : bool;

    (* enqueue on a given vproc *)
      define @enqueue-on-vproc-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : ();
    (* enqueue on a remote vproc *)
      define @enqueue-on-vproc (dst : vproc, fls : FLS.fls, k : PT.fiber) : ();

    )

  end *) = struct

    structure PT = PrimTypes
    structure O = Option

#include "vproc-queue.def"

    _primcode (

    (* returns true if the local queue is empty *)
      define @is-local-queue-empty-from-atomic (self : vproc) : bool =
	  let tl : queue_item = vpload (VP_RDYQ_TL, self)
	  if Equal(tl, Q_EMPTY) then
	      let hd : queue_item = vpload (VP_RDYQ_HD, self)
	      (* in *)
		if Equal(hd, Q_EMPTY)
		  then return (true)
		  else return (false)
	    else return (false)
	;

    (* returns true if the local queue contains more than one thread *)
      define @more-than-one-from-atomic (vp : vproc) : bool =
	  let tl : queue_item = vpload (VP_RDYQ_TL, vp)
	  let hd : queue_item = vpload (VP_RDYQ_HD, vp)

	  let nTl : int =
		    if Equal(tl, Q_EMPTY) then return(0)
		    else if Equal(SELECT(LINK_OFF, tl), Q_EMPTY) then return (1)
		    else return(2)

	  let nHd : int =
		    if Equal(hd, Q_EMPTY) then return(0)
		    else if Equal(SELECT(LINK_OFF, hd), Q_EMPTY) then return (1)
		    else return(2)

	  return(I32Gt(I32Add(nTl, nHd), 1))
	;

    (* reverse a non-empty queue.
     *   - fls fiber-local storage component of the first item of the input queue
     *   - k fiber component of the first item of the input queue
     *   - rest the rest of the queue
     * i.e., @reverse-queue(fls, k, (fls', k', Q_EMPTY)) ==> (fls', k', (fls, k, Q_EMPTY))
     *)
      define @reverse-queue (fls : FLS.fls, k : PT.fiber, rest : queue_item) : queue_item =
	  fun revQueue (fls : FLS.fls, k : PT.fiber, rest : queue_item, acc : queue_item) : queue_item =
	       let acc : queue_item = alloc(fls, k, acc)
	       if Equal(rest, Q_EMPTY)
		 then return(acc)
	       else
		 apply revQueue (SELECT(FLS_OFF, rest), SELECT(FIBER_OFF, rest), SELECT(LINK_OFF, rest), acc)
	  let qitem : queue_item = apply revQueue (fls, k, rest, Q_EMPTY)
	  return (qitem)
	;

      define @append (queue1 : queue_item, queue2 : queue_item) : queue_item =
	  fun append (queue1 : queue_item) : queue_item =
	      if Equal(queue1, Q_EMPTY)
		 then
		  return(queue2)
	      else 
		  let rest : queue_item = apply append(SELECT(LINK_OFF, queue1))
		  let queue11 : queue_item = alloc(SELECT(FLS_OFF, queue1), SELECT(FIBER_OFF, queue1), rest)
		  return(queue11)
	  apply append (queue1)
      ;

    (* enqueue on the local queue. NOTE: signals must be masked *)
      define inline @enqueue-from-atomic (vp : vproc, fls : FLS.fls, fiber : PT.fiber) : () =
	  let tl : queue_item = vpload (VP_RDYQ_TL, vp)
	  let qitem : queue_item = alloc(fls, fiber, tl)
	  do vpstore (VP_RDYQ_TL, vp, qitem)
	  return () 
	;

    (* retrieve items from the landing pad and put them on the local ready queue. *)
      define @unload-landing-pad-from-atomic (self : vproc) : () =
	  let landingPadItems : queue_item = VProc.@recv-from-atomic(self)

(*
TODO: differentiate between high- and low-priority queue items.

	(* splits the queue by priority. high goes first and low second. *)
	  fun split (queue : queue_item, highPrio : queue_item, lowPrio : queue_item) : [queue_item, queue_item] =
	      if Equal(queue, Q_EMPTY)
		 then 
		  let ret : [queue_item, queue_item] = alloc(highPrio, lowPrio)
		  return(ret)
	      else if I32Gt(SELECT(PRIORITY_OFF, queue), 0)
		 then (* high-priority queue item *)
		  let highPrio : queue_item = alloc(SELECT(FLS_OFF, queue), SELECT(FIBER_OFF, queue), highPrio)
		  apply split (SELECT(LINK_OFF, queue), highPrio, lowPrio)
	      else (* low-priority queue item *)
		  let lowPrio : queue_item = alloc(SELECT(FLS_OFF, queue), SELECT(FIBER_OFF, queue), lowPrio)
		  apply split (SELECT(LINK_OFF, queue), highPrio, lowPrio)
	  let split : [queue_item, queue_item] = apply split (landingPadItems, Q_EMPTY, Q_EMPTY)

          fun unloadLowPrio (queue : queue_item) : () =
	      if Equal(queue, Q_EMPTY)
		 then
		  return()
	      else
		  do @enqueue-from-atomic(self, SELECT(FLS_OFF, queue), SELECT(FIBER_OFF, queue))
		  apply unloadLowPrio(SELECT(LINK_OFF, queue))
	  do apply unloadLowPrio(#1(split))
*)

        (* put items off the landing pad at the front of the local ready queue *)
          let hd : queue_item = vpload (VP_RDYQ_HD, self)
	  let newHd : queue_item = @append(landingPadItems, hd)
	  do vpstore (VP_RDYQ_HD, self, newHd)
          
	  return()
      ;

   (* dequeue from the secondary list *)
      define @dequeue-slow-path (vp : vproc) : O.option =
          do @unload-landing-pad-from-atomic(vp)

	  let tl : queue_item = vpload (VP_RDYQ_TL, vp)
	  if Equal(tl, Q_EMPTY)
	    then return(O.NONE)
	    else
	      do vpstore (VP_RDYQ_TL, vp, Q_EMPTY)
	      let qitem : queue_item = @reverse-queue (
		      SELECT(FLS_OFF, tl), 
		      SELECT(FIBER_OFF, tl), 
		      (queue_item)SELECT(LINK_OFF, tl))
	      do vpstore (VP_RDYQ_HD, vp, (queue_item)SELECT(LINK_OFF, qitem))
	      return (O.SOME(qitem))
	;	  

    (* dequeue from the local queue  *)
      define inline @dequeue-from-atomic (vp : vproc) : O.option =
          do @unload-landing-pad-from-atomic(vp)

	  let hd : queue_item = vpload (VP_RDYQ_HD, vp)
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
	  do @enqueue-from-atomic (host_vproc, fls, fiber)
	  do SchedulerAction.@atomic-end(vp)
	  return ()
	;

    (* dequeue the first item to satisfy the given predicate  *)
      define @dequeue-with-pred-from-atomic (self : vproc, f : fun(FLS.fls / exh -> bool) / exh : exh) : O.option =
	  cont exit (x : O.option) = return(x)
	  let qitem : O.option = @dequeue-from-atomic(self)
	  case qitem
	   of O.NONE => throw exit(O.NONE)
	    | O.SOME (origItem : queue_item) =>
	      fun lp () : O.option =
		  let qitem : O.option = @dequeue-from-atomic(self)
		  case qitem
		   of O.NONE => throw exit(O.NONE)
		    | O.SOME(item : queue_item) =>
		      let b : bool = apply f (SELECT(FLS_OFF, item) / exh)
		      if b then throw exit (O.SOME(item))
		      else if Equal(SELECT(FLS_OFF, item), SELECT(FLS_OFF, origItem))
			then 
			  do @enqueue-from-atomic(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
			  throw exit (O.NONE)
			else 
			  do @enqueue-from-atomic(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
			  apply lp()
		  end
	      let b : bool = apply f (SELECT(FLS_OFF, origItem) / exh)
	      if b then throw exit(O.SOME(origItem))
	      else
		do @enqueue-from-atomic(self, SELECT(FLS_OFF, origItem), SELECT(FIBER_OFF, origItem))
		apply lp()
	  end
	;

    (* enqueue on a given vproc *)
      define inline @enqueue-on-vproc-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : () =
          if Equal(self, dst)
	     then
	      @enqueue-from-atomic(self, fls, k)
	  else
	      VProc.@send-from-atomic(self, dst, fls, k)
      ;

    (* enqueue on a remote vproc *)
      define inline @enqueue-on-vproc (dst : vproc, fls : FLS.fls, k : PT.fiber) : () =
	let self : vproc = SchedulerAction.@atomic-begin()
	do @enqueue-on-vproc-from-atomic(self, dst, fls, k)
	do SchedulerAction.@atomic-end(self)
	return()
      ;

    )

  end
