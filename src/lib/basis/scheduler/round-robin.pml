(* round-robin.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RoundRobin =
  struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode(

    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin (x : unit / exh : exh) : unit = 

	fun mkSwitch (self : vproc / exh : exh) : PT.sched_act = 

          (* variable to record the wall-clock time of the scheduling quantum *)
	    let currTime : ![long] = alloc (0:long)
            let currTime : ![long] = promote (currTime)

          (* list of sleeping threads local to the vproc; the third element of the thread object
	   * records the time at which the thread becomes ready to execute. *)
	    let sleeping : ![(* [FLS.fls, PT.fiber, long] *) List.list] = alloc (List.nil)
	    let sleeping : ![(* [FLS.fls, PT.fiber, long] *) List.list] = promote (sleeping)

	    fun addToSleepingList (fls : FLS.fls, fiber : PT.fiber, sleepDurationNs : long) : () =
		let timeToWake : long = I64Add (#0(currTime), sleepDurationNs)
                let newSleeping : List.list = promote (List.CONS (alloc (fls, fiber, timeToWake), #0(sleeping)))
		do #0(sleeping) := newSleeping
		return ()

	    (* check the sleeping list for threads that are ready to wake up. we move any such threads
	     * to the ready queue. *)
	    fun wakeupSleepingThreads () : () =
              (* update the current time *)
		let t : long = Time.@now ()
		do #0(currTime) := t
		fun f (slEnt : [FLS.fls, PT.fiber, long] / exh : exh) : bool =
		    if I64Gte (#2(slEnt), #0(currTime)) then return (true) else return (false)
	      (* the first list contains ready threads and second contains sleeping threads *)
		let res : [List.list, List.list] = PrimList.@partition (f, #0(sleeping) / exh)
		fun enq (thread : [FLS.fls, PT.fiber, long] / exh : exh) : () =
		    do VProcQueue.@enqueue-from-atomic (self, #0(thread), #1(thread))
		    return ()
		do PrimList.@app (enq, #0(res) / exh)
                let newSleeping : List.list = promote (#1(res))
		do #0(sleeping) := newSleeping
		return ()

            let spinWait : fun ( / -> bool) = SpinWait.@mk-spin-wait-fun (15)

	    cont switch (s : PT.signal) =

	      cont dispatch () =
		let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
		case item
		 of Option.NONE => 
		    let threadsOnLp : bool = VProcQueue.@poll-landing-pad-from-atomic (self)
		    case threadsOnLp
		     of true =>
			throw dispatch ()
		      | false =>
			let reset : bool = apply spinWait ()
			case reset
			 of true => 
			  (* sleep for at least 300 microseconds *)
			    let _ : bool = VProc.@nanosleep-from-atomic (self, 300000:long)
			    do apply wakeupSleepingThreads ()
			    throw dispatch ()
			  | false => 
			    throw dispatch ()
			end
		    end
		  | Option.SOME(qitem : VProcQueue.queue_item) =>
		    do SchedulerAction.@dispatch-from-atomic (self, switch, 
					      SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem))
		    throw dispatch ()
		end

	      case s
		of PT.STOP => 
		   throw dispatch ()
		 | PT.PREEMPT (k : PT.fiber) =>
		   let fls : FLS.fls = FLS.@get-in-atomic (self)
		   do VProcQueue.@enqueue-from-atomic (self, fls, k)
		   let _ : bool = VProcQueue.@poll-landing-pad-from-atomic (self)
		   do apply wakeupSleepingThreads ()
		   throw dispatch () 
		 | PT.SLEEP (k : PT.fiber, durationNs : long) =>
		   let fls : FLS.fls = FLS.@get-in-atomic (self)
                   do apply addToSleepingList (fls, k, durationNs)
                   throw dispatch ()
		 | _ =>
		   let e : exn = Match
		   throw exh(e)
	      end

           return (switch)

       (* run the scheduler on all vprocs *)
	do VProcInit.@bootstrap (mkSwitch / exh)
	return (UNIT)
      ;
    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = DEBUG("scheduler utils: initialized round-robin scheduler")

  end
