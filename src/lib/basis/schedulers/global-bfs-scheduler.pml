(* global-bfs-scheduler.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Scheduling policy for implicit threads:
 *   - makes a bread-first traversal of the spawn tree
 *   - uses a global FIFO scheduling queue
 *)

structure GlobalBFSScheduler (*  : 
  sig 
    val workGroup : unit -> ImplicitThread.group
  end *) = struct

    structure PT = PrimTypes

    _primcode (

    (* create a single worker *)
      define @worker (readyQ : LockedQueue.queue / exh : exh) : PT.fiber =
	cont schedulerLoop (s : PT.signal) =
          let self : vproc = host_vproc

	  cont dispatch () =
            do SchedulerAction.@yield-in-atomic(self)
	    let thd : Option.option = LockedQueue.@dequeue-from-atomic(readyQ)
	    case thd
	     of Option.NONE =>
		throw dispatch()
	      | Option.SOME(thd : ImplicitThread.thread) =>
		do ImplicitThread.@run-in-scheduler(self, schedulerLoop, thd / exh)
		throw dispatch()
	    end
	 case s
	  of PT.STOP =>
	     throw dispatch()
	   | PT.PREEMPT(k : PT.fiber) =>
	     (* mugging policy: allow other workers to steal k *)
	     (* QUESTION: does this policy work well for a shared FIFO queue? *)
	     let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
	     do LockedQueue.@enqueue-from-atomic(readyQ, thd)
	     throw dispatch()
	   | _ => 
	     let e : exn = Match
	     throw exh(e)
	 end

        cont initK (x : unit) =
	  throw schedulerLoop(PT.STOP)        (* initiate the scheduler loop *)

	return(initK)
      ;

    (* create the work group *)
      define @work-group (x : unit / exh : exh) : ImplicitThread.group =
	let readyQ : LockedQueue.queue = LockedQueue.@new()
	let init : PT.fiber = @worker(readyQ / exh)
	fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
            let vp : vproc = SchedulerAction.@atomic-begin()
	    do LockedQueue.@enqueue-from-atomic(readyQ, thd)
            do SchedulerAction.@atomic-end(vp)
	    return(UNIT)
      (* provide no facility for removing a thread from the ready queue *)
        fun removeFn (thd : ImplicitThread.thread / exh : exh) : bool = return(true)
	let group : ImplicitThread.group = ImplicitThread.@group(init, spawnFn, removeFn, enum(0) / exh)
	return(group)
      ;

    )

    val workGroup : unit -> ImplicitThread.group = _prim(@work-group)

  end
