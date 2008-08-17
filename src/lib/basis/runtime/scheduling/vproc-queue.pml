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

#define Q_EMPTY (queue)enum(0)

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
		 apply revQueue (#0(lst), #1(lst), #2(lst), acc / exh)
	let qitem : queue = apply revQueue (fls, k, lst, rest / exh)
	return (qitem)
      ;

      extern void *VProcDequeue (void *) __attribute__((alloc));

   (* dequeue from the secondary list *)
      define @dequeue-slow-path (vp : vproc / exh : PT.exh) : queue =
	cont loop () =
	   let tl : queue = vpload (VP_RDYQ_TL, vp)
           if Equal(tl, Q_EMPTY)
              then
		 let sleepKOpt : Option.option = ccall VProcDequeue(vp)
		 case sleepKOpt
		  of NIL => 
		     (* the queue is nonempty, try again *)
		     throw loop ()
		   | SOME (sleepK : queue) =>
		     (* return a fiber that will put the vproc to sleep *)
		     return (sleepK)
		 end
           else
		do vpstore (VP_RDYQ_TL, vp, Q_EMPTY)
		let qitem : queue = @reverse-queue (#0(tl), #1(tl), (queue)#2(tl), Q_EMPTY / exh)
		let item : [FLS.fls, PT.fiber, queue] = ([FLS.fls, PT.fiber, queue]) qitem
		let link : queue = #2 (tl)
		do vpstore (VP_RDYQ_HD, vp, link)
		return (qitem)

	throw loop ()
      ;	  

    (* dequeue from the vproc's thread queue *)
      define @dequeue ( / exh : PT.exh) : queue =
        let vp : vproc = host_vproc
	let hd : queue = vpload (VP_RDYQ_HD, vp)
        if Equal(hd, Q_EMPTY)
           then
	     (* the primary list is empty, so try the secondary list *)
	    let item : queue = @dequeue-slow-path (vp / exh)
	    return (item)
        else 
	     (* got a thread from the primary list *)
	     do vpstore (VP_RDYQ_HD, vp, #2(hd))
	     return (hd)
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

   (* enqueue on a remote vproc *)
      define @enqueue-on-vproc (dst : vproc, fls : FLS.fls, k : PT.fiber / exh : PT.exh) : () =
	let k : PT.fiber = promote (k)
	do ccall EnqueueOnVProc(host_vproc, dst, fls, k)
	return ()
      ;

      define @ex (x : PT.unit / exh : PT.exh) : PT.unit =
        cont k2 (x : PT.unit) =
          do ccall M_Print ("original thread\n")
          return(UNIT)
        cont k1 (x : PT.unit) =
          do ccall M_Print ("queue example\n")
          return(UNIT)
        let fls : FLS.fls = FLS.@get(/exh)
        do @enqueue(fls, k1 / exh)
        do Control.@forward ((any)k2 / exh)
        do assert(FALSE)
	return(UNIT)
      ;

    )

    val ex : unit -> unit = _prim(@ex)
(*    val _ = ex()*)

  end
