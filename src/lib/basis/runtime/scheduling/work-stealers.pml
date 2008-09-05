(* work-stealers.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * A top-level work-stealing scheduler for threads.
 *
 * This scheduler implements both a round-robin policy for local threads and a load balancing scheme
 * for distributing threads to other vprocs.  The protocol for this scheduler is below. 
 *    (0) Make a list of all other vprocs starting after the current vproc.
 *    (1) If the local vproc queue has a thread ready, run it.
 *    (2) Otherwise, go into steal mode:
 *        (2.1) Send out a thief thread to the first vproc.
 *        (2.2) If a thread is available, steal it and enqueue it on the idle vproc.
 *        (2.3) Otherwise, do the same for the other vprocs.
 *    (3) If the other vprocs have nothing available to steal, add the current vproc to the sleeping list
 *        and go to sleep.
 *    (4) When a preemption arrives, check the local ready queue.  If several unpinned threads are
 *        available, then wake some vprocs.  Then run the next thread in the ready queue.
 *)

#include "vproc-queue.def"

structure WorkStealers =
  struct

    structure PT = PrimTypes
    structure VPQ = VProcQueue
    structure O = Option

    _primcode(

      define @mk-sched-act (self : vproc / exh : PT.exh) : PT.sigact =
	  fun wakeSleeping (/ exh : PT.exh) : () =
	      cont wakeupK (_ : PT.unit) = Control.@stop(/ exh)
	      fun f (vp : vproc / exh : PT.exh) : () =
		  let idle : PT.bool = vpload(VP_IDLE, vp)
		  if idle
		     then VPM.@send(vp, wakeupK / exh)
		  else return()
	      SchedulerUtils.@for-other-vprocs(f / exh)

	(* (2) *)
	  fun stealMode (vps : List.list / exh : PT.exh) : () =
	      case vps
	       of List.NIL =>
		  do print_debug("work-stealers: failed to get work")
		(* (3) *)
		  apply markAsleep(self / exh)
		| List.CONS(vp : vproc, vps : List.list) =>
		  cont tryNext () =
		    do print_debug("work-stealers: try next")
		    do apply stealMode(vps / exh)
		    Control.@stop(/ exh)
		(* (2.1) *)
		  do print_debug("work-stealers: sending thief to vproc")
		  cont thiefK (x : PT.unit) =
		    do print_debug("work-stealers: in thiefK")
		    let hasElts : PT.bool = VPQ.@is-queue-gt-one(/ exh)
		    if hasElts
		       then 
			do print_debug("work-stealers: dequeuing")
			let item : Option.option = VPQ.@dequeue(/ exh)
			case item
			 of O.NONE =>
			    throw tryNext()
			  | O.SOME (item : VPQ.queue) =>
			  (* (2.2) *)
			    do print_debug("work-stealers: sending thread to idle vproc")
			    do VPQ.@enqueue-on-vproc(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item) / exh)
			    Control.@stop(/ exh)
			end
		    else throw tryNext()
		VPM.@send(vp, thiefK / exh)
	      end

	cont impossible() = 
	  do assert(PT.FALSE)
	  return($0)

	cont switch (sign : PT.signal) =
	  cont dispatch () =
	  (* if there are stealable elements on the queue, wake any sleeping vprocs *)
	    let hasElts : PT.bool = VPQ.@is-queue-gt-one(/ exh)
	    do if hasElts
	       then apply wakeSleepingVProcs(/ exh)
	       else return()
	    let item : Option.option = VPQ.@dequeue(/ exh)
	    case item
	     of NONE => 
		let potentialVictims : List.list = SchedulerUtils.@other-vprocs(/ exh)
		do apply stealMode(potentialVictims / exh)
		do @wait(/ exh)
		throw dispatch()
	      | Option.SOME(qitem : VPQ.queue) =>
		do Control.@run-thread (switch, SELECT(FIBER_OFF, qitem), SELECT(FIBER_OFF, qitem) / exh)
		throw impossible()
	    end
	  case sign
	   of PT.STOP => throw dispatch()
	    | PT.PREEMPT(k : PT.fiber) => 
	      let fls : FLS.fls = FLS.@get ( / exh)
	      do VProcQueue.@enqueue (fls, k / exh)
	      throw dispatch () 
	    | PT.SUSPEND (k : PT.fiber, retK : PT.cont) =>
	      let fls : FLS.fls = FLS.@get ( / exh)
	      cont retK' (x : PT.unit) =
	      throw retK(k)
	      do VProcQueue.@enqueue (fls, k / exh)
	      throw dispatch () 
	    | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, fls : FLS.fls) =>
	      do VProcQueue.@enqueue (fls, k / exh)
	      do Control.@run(switch, retK / exh)
	      throw impossible()
	  end
        return(switch)
      ;

      define @work-stealers(_ : PT.unit / exh : PT.exh) : PT.unit = 
	fun mkSwitch (self : vproc / exh : PT.exh) : PT.sigact = 
	    @mk-sched-act(self / exh)
      (* fiber-local storage for the top-level scheduler *)
	let fls : FLS.fls = FLS.@new (UNIT / exh)
      (* run the scheduler on all vprocs *)
	do @boot-default-scheduler (mkSwitch, fls / exh)
	return(UNIT)
      ;

    )

    val workStealers : unit -> unit = _prim(@work-stealers)
    val _ = workStealers()
    val _ = Print.printLn "work-stealers: initialized on vprocs"

  end
