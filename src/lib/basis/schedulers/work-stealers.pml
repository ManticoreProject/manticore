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
    structure VPM = VProcMessenger
    structure O = Option
    structure L = List
    structure FLS = FiberLocalStorage

    _primcode(

    (* wake up any sleeping vprocs by enqueuing dummy threads *)
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
	(* try to steal a thread from a remote vproc *)
	  fun thief (vps : L.list / exh : PT.exh) : () =
		do print_msg ("work-stealers: steal mode")
		let noStealableFibers : PT.bool = VPQ.@is-local-queue-empty(host_vproc)
		if noStealableFibers
		  then apply sendThieves (vps / exh)
		  else
		    do print_msg ("work-stealers: dequeuing")
		    fun isNotPinned (fls : FLS.fls / exh : PT.exh) : PT.bool =
			let b : PT.bool = FLS.@is-pinned(fls / exh)
			return (NotEqual(b, PT.true))
		    let item : O.option = VPQ.@dequeue-with-pred-in-atomic (isNotPinned / exh)
		    case item
		     of O.NONE =>
			  apply sendThieves (vps / exh)
		      | O.SOME (item : VPQ.queue) =>
			  do print_msg("work-stealers: sending thread to idle vproc")
			  VPQ.@enqueue-on-vproc(self, SELECT(FLS_OFF, item), SELECT(FIBER_OFF, item))
		    end
	(* send thieves to all other vprocs *)
	  and sendThieves (vps : L.list / exh : PT.exh) : () =
	      do print_msg("work-stealers: try next")
	      case vps
	       of nil =>
		  do print_msg("work-stealers: failed to get work")
		  return()
		| L.CONS(vp : vproc, vps : L.list) =>
		  let idle : int = vpload(VP_IDLE, vp)
		  do if I32Eq(idle, 1)
			then apply sendThieves(vps / exh)
		     else return()
		  cont thiefK (x : PT.unit) =
		    do apply thief(vps / exh)
		    let _ : PT.unit = Control.@stop(/ exh)
		    return()
		VPM.@send(vp, thiefK / exh)
	      end
	  cont impossible() = 
	      do assert(PT.false)
	      return($0)
	  cont switch (sign : PT.signal) =
	      cont dispatch () =
	      (* notify any idle vprocs if there is extra local work *)
		let hasElts : PT.bool = VPQ.@is-local-queue-gt-one(/ exh)
		do if hasElts
		   then @wake-sleeping-vprocs(/ exh)
		   else return()
		let item : O.option = VPQ.@dequeue(/ exh)
		case item
		 of O.NONE => 
		  (* try to steal a thread *)
		    let potentialVictims : L.list = SchedulerUtils.@other-vprocs(/ exh)
		    do apply sendThieves(potentialVictims / exh)
		    do SchedulerUtils.@wait(/ exh)
		    throw dispatch()
		  | O.SOME(qitem : VPQ.queue) =>
		    do Control.@run-thread (switch, SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem) / exh)
		    throw impossible()
		end
	  (* in *)
	    case sign
	     of PT.STOP => 
		throw dispatch()
	      | PT.PREEMPT(k : PT.fiber) => 
		let fls : FLS.fls = FLS.@get ()
		do VPQ.@enqueue (fls, k / exh)
		throw dispatch () 
	      | _ =>
		let e : exn = Match
		throw exh(e)
	    end
	  return (switch)
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
    val _ = DEBUG("work-stealers: initialized on vprocs")

  end
