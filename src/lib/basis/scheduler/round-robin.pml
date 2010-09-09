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
	    let sleeping : ![(* [FLS.fls, PT.fiber, ml_long] *) List.list] = alloc (List.nil)
	    let sleeping : ![(* [FLS.fls, PT.fiber, ml_long] *) List.list] = promote (sleeping)

	    fun addToSleepingList (fls : FLS.fls, fiber : PT.fiber, sleepDurationNs : long) : () =
		let timeToWake : ml_long = alloc(I64Add (#0(currTime), sleepDurationNs))
                let newSleeping : List.list = promote (CONS (alloc (fls, fiber, timeToWake), #0(sleeping)))
		do #0(sleeping) := newSleeping
		return ()

	    (* check the sleeping list for threads that are ready to wake up. we move any such threads
	     * to the ready queue. we return true when we have moved any thread to the ready queue. *)
	    fun wakeupSleepingThreads () : bool =
              (* update the current time *)
		let t : long = Time.@now ()
		do #0(currTime) := t
		fun f (slEnt : [FLS.fls, PT.fiber, ml_long] / exh : exh) : bool =
		    if I64Lt (#0(#2(slEnt)), t) then return (true) else return (false)
	      (* the first list contains ready threads and second contains sleeping threads *)
		let res : [List.list, List.list] = PrimList.@partition (f, #0(sleeping) / exh)
		fun enq (thread : [FLS.fls, PT.fiber, ml_long] / exh : exh) : () =
		    do VProcQueue.@enqueue-from-atomic (self, #0(thread), #1(thread))
		    return ()
		do PrimList.@app (enq, #0(res) / exh)
                let newSleeping : List.list = promote (#1(res))
		do #0(sleeping) := newSleeping
		case #0(res)
		 of List.nil => return (false)
		  | CONS (_ : any, r : List.list) => return (true)
                end

	  (* 
	    Here we wait until either a thread wakes from a sleeping state or an ready thread
	    has been placed on the landing pad by a remote vproc.

	    void waitForWork () 
	    {
	      while(1) {
		for (int i = 0; i < 2000; i++) {
		  if (pollLandingPad ())
		    return;  // there is an incoming ready thread
		  Pause();  // reduce power consumption
		  for (int j = 0; j < 500; j++); // spin for a while
		}
		if (wakeupSleepingThreads ())
		  return;  // some thread is ready to wake up
		/* sleep for one millisecond by calling into the OS */
		VProcNanosleep (1ms);
	      }
	    }

	   *)
	    fun waitForWork () : () =
		cont workIsAvailable () = return ()  (* leave the waitForWork loop *)
		fun lp1 (i : int) : () =
		    let w : bool = VProcQueue.@poll-landing-pad-from-atomic (self)
		    do case w
			of true => throw workIsAvailable ()
			 | false => return ()
                       end
		    do Pause ()
		    fun lp2 (j : int) : () = 
			if I32Gt (j, 500) then return () else apply lp2 (I32Add (j, 1))
		    do apply lp2 (0)
		    if I32Gt (i, 2000) then return () else apply lp1 (I32Add (i, 1))
		do apply lp1 (0)
		let w : bool = apply wakeupSleepingThreads ()
		do case w
		    of true => throw workIsAvailable ()
		     | false => return ()
		   end
		let _ : bool = VProc.@nanosleep-from-atomic (self, 1000000:long)
                apply waitForWork ()

	    cont switch (s : PT.signal) =
	     (* pick the next thread to run *)
		cont dispatch () =
		  let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
		  case item
		   of Option.NONE => 
		      do apply waitForWork ()
		      throw dispatch ()
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
		   let _ : bool = apply wakeupSleepingThreads ()
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
