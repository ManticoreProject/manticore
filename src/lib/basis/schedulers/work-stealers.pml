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
      define @wake-idle-vprocs (/ exh : exh) : () =
        cont wakeupK (_ : unit) = 
	      let _ : unit = SchedulerAction.@stop()
	      return()
	fun f (vp : vproc / exh : exh) : () =
	    let idle : bool = vpload(VP_IDLE, vp)
	    if idle
	       then 
		let fls : FLS.fls = FLS.@get()
		VPQ.@enqueue-on-vproc(vp, fls, wakeupK)
	    else return()
	VProc.@for-other-vprocs(f / exh)
      ;

      define @mk-sched-act (self : vproc / exh : exh) : PT.sched_act =

	  cont impossible () = 
	    let e : exn = Fail(@"WorkStealers.@mk-sched-act: impossible")
            throw exh(e)

	(* try to steal a thread from a remote vproc *)
	  fun thief (victimVP : vproc, vps : L.list / exh : exh) : () =
		let noStealableFibers : bool = VPQ.@is-local-queue-empty-from-atomic(victimVP)
		if noStealableFibers
		  then apply sendThieves (vps / exh)
		  else
		    fun isNotPinned (fls : FLS.fls / exh : exh) : bool =
			let pinned : int = FLS.@pin-info(fls / exh)
                        let victim : int = VProc.@vproc-id(victimVP)
			return (NotEqual(pinned, victim))
		    let item : O.option = VPQ.@dequeue-with-pred-from-atomic (victimVP, isNotPinned / exh)
		    case item
		     of O.NONE =>
			  PRINT_DEBUG("WorkStealers.thief: failed to steal work")
			  apply sendThieves (vps / exh)
		      | O.SOME (item : VPQ.queue) =>
			  PRINT_DEBUG("WorkStealers.thief: sending stolen work back to original vproc")
			  VPQ.@enqueue-on-vproc(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
		    end

	(* send thieves to all other vprocs *)
	  and sendThieves (vps : L.list / exh : exh) : () =
	      case vps
	       of nil =>
		  PRINT_DEBUG("WorkStealers.sendThieves: no stealable work on other vprocs")
		  return()
		| L.CONS(vp : vproc, vps : L.list) =>
		  let idle : int = vpload(VP_IDLE, vp)
		  do if I32Eq(idle, 1)
			then apply sendThieves(vps / exh)
		     else return()
		  cont thiefK (x : unit) =
                    let victimVP : vproc = SchedulerAction.@atomic-begin()
                    do assert(Equal(victimVP, vp))
		    do apply thief(victimVP, vps / exh)
		    let _ : unit = SchedulerAction.@stop-from-atomic(victimVP)
		    return()
                let vpId : int = VProc.@vproc-id(vp)
              (* prevent the victim from migrating the thief to another vproc *)
                let thiefFLS : FLS.fls = FLS.@new-pinned(vpId)
		VPQ.@enqueue-on-vproc(vp, thiefFLS, thiefK)
	      end

	  cont switch (sign : PT.signal) =
	      cont dispatch () =
	      (* notify any idle vprocs if there is extra local work *)
		let hasElts : bool = VPQ.@more-than-one-from-atomic(self)
		do if hasElts
		   then @wake-idle-vprocs(/ exh)
		   else return()
		let item : O.option = VPQ.@dequeue-from-atomic(self)
		case item
		 of O.NONE => 
		  (* try to steal a thread *)
		    let potentialVictims : L.list = VProc.@other-vprocs(/ exh)
		    do apply sendThieves(potentialVictims / exh)
		    do VProc.@wait-in-atomic()
		    throw dispatch()
		  | O.SOME(qitem : VPQ.queue) =>
		    do SchedulerAction.@dispatch-from-atomic (self, switch, SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem))
		    throw impossible()
		end
	  (* in *)
	    case sign
	     of PT.STOP => 
		throw dispatch()
	      | PT.PREEMPT(k : PT.fiber) => 
		let fls : FLS.fls = FLS.@get ()
		do VPQ.@enqueue-from-atomic (self, fls, k)
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
	do VProc.@boot-default-scheduler (mkSwitch / exh)
	return(UNIT)
      ;

    )

    val workStealers : unit -> unit = _prim(@work-stealers)
    val _ = workStealers()
    val _ = DEBUG("work-stealers: initialized on vprocs")

  end
