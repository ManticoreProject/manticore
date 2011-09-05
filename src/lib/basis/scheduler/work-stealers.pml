(* work-stealers.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * A top-level work-stealing scheduler for threads.
 *
 * This scheduler implements both a round-robin policy for local threads and a load balancing scheme
 * for distributing threads to other vprocs.  Below is the protocol for this scheduler.
 *    (1) If the local vproc queue has a thread ready, run it.
 *    (2) Otherwise, for each remote vproc, do the following.
 *        (2.1) Send out a thief thread to a remote vproc.
 *        (2.2) If a thread is available, send it back to the original vproc.
 *    (3) If all steal attemps were unsuccessful, go to sleep.
 *    (4) Frequently check the local queue for excess work, and if some exists wake up any idle
 *        vprocs.
 *
 * NOTE: what should happen in the following situaiton? A vproc is waiting for a thief to
 * make its rounds, when a thread appears on that vproc's ready queue.
 *)

#include "vproc-queue.def"

structure WorkStealers =
  struct

    structure PT = PrimTypes
    structure VPQ = VProcQueue
    structure O = Option
    structure L = List

    _primcode(

    (* wake up any idle vprocs by enqueuing dummy threads on them *)
      define @wake-idle-vprocs (self : vproc / exh : exh) : () =
	fun f (vp : vproc / exh : exh) : () =
            do VProc.@preempt-in-atomic(self, vp)
            return()
	do VProc.@for-other-vprocs-in-atomic(self, f / exh)
        return()
      ;

      define @mk-sched-act (self : vproc / exh : exh) : PT.sched_act =

	  cont impossible () = 
	    let e : exn = Fail(@"WorkStealers.@mk-sched-act: impossible")
            throw exh(e)

	(* try to steal a thread from a remote vproc *)
	  fun thief (victimVP : vproc, vps : L.list) : () =
		let noStealableFibers : bool = VPQ.@is-local-queue-empty-in-atomic(victimVP)
		if noStealableFibers
		  then 
		    PRINT_DEBUG("WorkStealers.thief: no available fibers")
		    apply sendThieves (vps)
		  else
		    fun isNotPinned (fls : FLS.fls / exh : exh) : bool =
			let pinned : int = FLS.@pin-info(fls / exh)
                        let victim : int = VProc.@vproc-id(victimVP)
			return (NotEqual(pinned, victim))
		    let item : O.option = VPQ.@dequeue-with-pred-in-atomic (victimVP, isNotPinned / exh)
		    case item
		     of O.NONE =>
			  PRINT_DEBUG("WorkStealers.thief: failed to steal work")
			  apply sendThieves (vps)
		      | O.SOME (item : VPQ.queue_item) =>
			  PRINT_DEBUG("WorkStealers.thief: sending stolen work back to original vproc")
			  do VPQ.@enqueue-on-vproc(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
			  return()
		    end

	(* send thieves to all other vprocs *)
	  and sendThieves (vps : L.list) : () =
	      case vps
	       of nil =>
		  PRINT_DEBUG("WorkStealers.sendThieves: no available work on other vprocs")
		  return()
		| CONS(victimVPPtr : [vproc], vps : L.list) =>
		  PRINT_DEBUG("WorkStealers.sendThieves: sending a thief")
                  let victimVP : vproc = #0(victimVPPtr)
		  cont thiefK (x : unit) =
                    let victimVP : vproc = SchedulerAction.@atomic-begin()
		    do apply thief(victimVP, vps)
		    let _ : unit = SchedulerAction.@stop-from-atomic(victimVP)
		    return()
                let vpId : int = VProc.@vproc-id(victimVP)
              (* prevent the victim from migrating the thief to another vproc *)
                let thiefFLS : FLS.fls = FLS.@new-pinned(vpId)
		do VPQ.@enqueue-on-vproc(victimVP, thiefFLS, thiefK)
	        do VProc.@preempt-in-atomic(self, victimVP)
                return()
	      end

	  cont switch (sign : PT.signal) =
	      cont dispatch () =
	      (* notify any idle vprocs if there is extra local work *)
		let hasElts : bool = VPQ.@more-than-one-in-atomic(self)
		do case hasElts
		    of true => @wake-idle-vprocs(self / exh)
		       | false => return()
                   end
		let item : O.option = VPQ.@dequeue-in-atomic(self)
		case item
		 of O.NONE => 
		    let potentialVictims : L.list = VProc.@other-vprocs-in-atomic(self / exh)
		    do apply sendThieves(potentialVictims)
		    do VProc.@sleep-in-atomic(self)
		    throw dispatch()
		  | O.SOME(qitem : VPQ.queue_item) =>
		    do SchedulerAction.@dispatch-from-atomic (self, switch, SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem))
		    throw impossible()
		end
	  (* in *)
	    case sign
	     of PT.STOP => 
		throw dispatch()
	      | PT.PREEMPT(k : PT.fiber) => 
		let fls : FLS.fls = FLS.@get-in-atomic (self)
		do VPQ.@enqueue-in-atomic (self, fls, k)
		throw dispatch () 
	      | _ =>
		let e : exn = Match
		throw exh(e)
	    end
	  return (switch)
	;

      define @work-stealers(x : unit / exh : exh) : unit = 
	fun mkSwitch (self : vproc / exh : exh) : PT.sched_act = @mk-sched-act(self / exh)
      (* run the scheduler on all vprocs *)
	do VProcInit.@bootstrap (mkSwitch / exh)
	return(UNIT)
      ;

    )

    val workStealers : unit -> unit = _prim(@work-stealers)
    val _ = workStealers()
    val _ = DEBUG("work-stealers: initialized on vprocs")

  end
