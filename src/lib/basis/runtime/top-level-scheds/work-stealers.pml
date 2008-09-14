(* work-stealers.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * A top-level work-stealing scheduler for threads.
 *
 * This scheduler implements both a round-robin policy for local threads and a load balancing scheme
 * for distributing threads to other vprocs.  The protocol for this scheduler is below. 
 *    (1) If the local vproc queue has a thread ready, run it.
 *    (2) Otherwise, go into steal mode:
 *        (2.1) Send out a thief thread to the first vproc.
 *        (2.2) If a thread is available, steal it and enqueue it on the idle vproc.
 *        (2.3) Otherwise, do the same for the other vprocs.
 *    (3) If the other vprocs have nothing available to steal, go to sleep.
 *    (4) When a preemption arrives, check the local ready queue.  If several unpinned threads are
 *        available, then wake some vprocs.  Then run the next thread in the ready queue.
 *)

#include "vproc-queue.def"

structure WorkStealers =
  struct

    structure PT = PrimTypes
    structure VPQ = VProcQueue
    structure VPM = VProcMessenger
    structure O = Option
    structure L = List
    structure FLS = FiberLocalStorage

    _primcode(

      define @wake-sleeping-vprocs (/ exh : PT.exh) : () =
        cont wakeupK (_ : PT.unit) = 
	      let _ : PT.unit = Control.@stop(/ exh)
	      return()
	    fun f (vp : vproc / exh : PT.exh) : () =
		let idle : PT.bool = vpload(VP_IDLE, vp)
		if idle
		   then VPM.@send(vp, wakeupK / exh)
		else return()
	    SchedulerUtils.@for-other-vprocs(f / exh)
      ;

      define @mk-sched-act (self : vproc / exh : PT.exh) : PT.sigact =
        cont waitK(x : PT.unit) = 
          do SchedulerUtils.@wait(/ exh)
          let _ : PT.unit = Control.@stop(/ exh)
          return($0)

	fun stealMode (vps : L.list / exh : PT.exh) : () =
	    do print_msg("work-stealers: steal mode")
	    do ccall M_PrintPtr("self", self)
	    let hasStealableElts : PT.bool = VPQ.@is-local-queue-geq-one(/ exh)
	    if hasStealableElts
 	       then 
		do print_msg("work-stealers: dequeuing")
                fun isNotPinned (fls : FLS.fls / exh : PT.exh) : PT.bool =
		    let b : PT.bool = FLS.@is-pinned(fls / exh)
                    return(NotEqual(b, PT.TRUE))
                let item : O.option = VPQ.@dequeue-with-pred(isNotPinned / exh)
		case item
		 of O.NONE =>
		    apply tryNext(vps / exh)
		  | O.SOME (item : VPQ.queue) =>
		    do print_msg("work-stealers: sending thread to idle vproc")
		    do VPQ.@enqueue-on-vproc(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item) / exh)
                    return()
		end
	    else apply tryNext(vps / exh)

      and tryNext (vps : L.list / exh : PT.exh) : () =
	  do print_msg("work-stealers: try next")
	  case vps
	   of L.NIL =>
	      do print_msg("work-stealers: failed to get work")
	      return()
	    | L.CONS(vp : vproc, vps : L.list) =>
	      let idle : int = vpload(VP_IDLE, vp)
	      do if I32Eq(idle, 1)
		    then apply tryNext(vps / exh)
		 else return()
	      cont thiefK (x : PT.unit) =
		do apply stealMode(vps / exh)
		let _ : PT.unit = Control.@stop(/ exh)
                return()
	    VPM.@send(vp, thiefK / exh)
	  end

	cont impossible() = 
	  do assert(PT.FALSE)
	  return($0)

	cont switch (sign : PT.signal) =
	  cont dispatch () =
	    let hasElts : PT.bool = VPQ.@is-local-queue-gt-one(/ exh)
	    do if hasElts
	       then @wake-sleeping-vprocs(/ exh)
	       else return()
	    let item : O.option = VPQ.@dequeue(/ exh)
	    case item
	     of O.NONE => 
do print_ppt()
		let potentialVictims : L.list = SchedulerUtils.@other-vprocs(/ exh)
                do apply tryNext(potentialVictims / exh)
                do SchedulerUtils.@wait(/ exh)
                throw dispatch()
	      | O.SOME(qitem : VPQ.queue) =>
		do Control.@run-thread (switch, SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem) / exh)
		throw impossible()
	    end
	  case sign
	   of PT.STOP => throw dispatch()
	    | PT.PREEMPT(k : PT.fiber) => 
	      let fls : FLS.fls = FLS.@get ( / exh)
	      do VPQ.@enqueue (fls, k / exh)
	      throw dispatch () 
	    | PT.SUSPEND (k : PT.fiber, retK : PT.cont) =>
	      let fls : FLS.fls = FLS.@get ( / exh)
	      cont retK' (x : PT.unit) =
	      throw retK(k)
	      do VPQ.@enqueue (fls, k / exh)
	      throw dispatch () 
	    | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, fls : FLS.fls) =>
	      do VPQ.@enqueue (fls, k / exh)
	      do Control.@run(switch, retK / exh)
	      throw impossible()
	  end
        return(switch)
      ;

      define @work-stealers(x : PT.unit / exh : PT.exh) : PT.unit = 
	fun mkSwitch (self : vproc / exh : PT.exh) : PT.sigact = @mk-sched-act(self / exh)
      (* run the scheduler on all vprocs *)
	do SchedulerUtils.@boot-default-scheduler (mkSwitch / exh)
	return(UNIT)
      ;

    )

    val workStealers : unit -> unit = _prim(@work-stealers)
    val _ = workStealers()
    val _ = printMsg("work-stealers: initialized on vprocs")

  end
