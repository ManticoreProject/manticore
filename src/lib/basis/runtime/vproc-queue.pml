(* vproc-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module contains accessor functions for VProc thread queues. Each VProc owns
 * a single queue, and only the enqueue operation is available to other VProcs --
 * dequeue operations are local. For performance reasons, we represent queues as three
 * linked lists: the primary and secondary queue and the landing pad. We can dequeue
 * locally from the primary queue, enqueue locally from the secondary queue, and enqueue
 * remotely from the landing pad.
 *)

#define Q_EMPTY_B enum(0):queue

structure VProcQueue =
  struct

    structure FLS = FiberLocalStorage
    structure PT = PrimTypes

  (* vproc queue structure *)
    type data = _prim ( [FLS.fls, PT.fiber, any] )
    datatype queue
      = Q_EMPTY
      | Q_ITEM of data

    _primcode(

    (* takes a queue element (the first three arguments), a queue (lst), and another queue (rest), and 
     * produces the following queue:
     *    rev(lst) -> rev(rest) -> (first three arguments)
     *)
      define @reverse-queue (fls : FLS.fls, k : PT.fiber, lst : queue, rest : queue / exh : PT.exh) : queue =
	fun revQueue (fls : FLS.fls, k : PT.fiber, lst : queue, acc : queue / exh : PT.exh) : queue =
	     let acc : queue = Q_ITEM (alloc(fls, k, acc))
	      case lst of 
		  Q_EMPTY_B => return (acc)
		| Q_ITEM (item:[FLS.fls, PT.fiber, queue]) =>
		   apply revQueue (#0(item), #1(item), #2(item), acc / exh)
	      end
	let qitem : queue = apply revQueue (fls, k, lst, rest / exh)
	return (qitem)
      ;

      (* TODO: move this function to BOM *)
      extern void *VProcDequeue (void *) __attribute__((alloc));

   (* obtain elements from the secondary queue *)
      define @dequeue-slow-path (vp : vproc / exh : PT.exh) : queue =
	cont loop () =
	   let tl : queue = vpload (VP_RDYQ_TL, vp)
	   case tl of
	      Q_EMPTY_B =>
		 let sleepKOpt : option = ccall VProcDequeue(vp)
		 case sleepKOpt
		  of NIL => 
		     (* the queue is nonempty, try again *)
		     throw loop ()
		   | SOME (sleepK : queue) =>
		     (* return a fiber that will put the vproc to sleep *)
		     return (sleepK)
		 end
	    | Q_ITEM (item:[FLS.fls, PT.fiber, queue]) =>
		do vpstore (VP_RDYQ_TL, vp, Q_EMPTY_B)
		let qitem : queue = hlop @reverse-queue (#0(item), #1(item), #2(item), Q_EMPTY_B / exh)
		let item : [FLS.fls, PT.fiber, queue] = ([FLS.fls, PT.fiber, queue]) qitem
		let link : queue = #2 (item)
		do vpstore (VP_RDYQ_HD, vp, link)
		return (qitem)
	   end
	throw loop ()
      ;	  

    (* dequeue an element from the vproc's thread queue *)
      define @dequeue (vp : vproc / exh : PT.exh) : queue =
	let hd : queue = vpload (VP_RDYQ_HD, vp)
	case hd
	  of QITEM (_, _, link:queue) => 
	     (* got an element from the primary queue *)
	     do vpstore (VP_RDYQ_HD, vp, link)
	     return (hd)
	   | Q_EMPTY_B => 
	     (* primary queue is empty, so try the secondary queue *)
	     let item : queue = @dequeue-slow-path (vp / exh)
	     return (item)
	end
      ;

    (* enqueue an element on the vproc's thread queue *)
      define @enqueue (vp : vproc, fls : PT.fls, fiber : PT.fiber / exh : PT.exh) : () =
	 let tl : queue = vpload (VP_RDYQ_TL, vp)
	 let qitem : queue = QITEM (fls, fiber, tl)
	 do vpstore (VP_RDYQ_TL, vp, qitem)
	 return () 
      ;

    )

val _ = print "queue\n"

  end
