(* global-bfs-scheduler.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Scheduling policy for implicit threads:
 *   - makes a bread-first traversal of the spawn tree
 *   - uses a global FIFO scheduling queue
 *)

structure GlobalBFSScheduler : 
  sig 
    val workGroup : unit -> ImplicitThread.group
  end = struct

    structure PT = PrimTypes

    _primcode (

    (* create a single worker *)
      define @worker (readyQ : LockedQueue.queue / exh : exh) : PT.fiber =
	cont schedulerLoop (s : PT.signal) =
	  cont dispatch () =
	    let thd : Option.option = LockedQueue.@dequeue(readyQ / exh)
	    case thd
	     of Option.NONE =>
		let _ : PT.unit = SchedulerAction.@yield-in-atomic(host_vproc)
		throw dispatch()
	      | Option.SOME(thd : ImplicitThread.thread) =>
		do ImplicitThread.@run(schedulerLoop, thd / exh)
		throw dispatch()
	    end
	 case s
	  of PT.STOP =>
	     throw dispatch()
	   | PT.PREEMPT(k : PT.fiber) =>
	     (* mugging policy: allow other workers to steal k *)
	     (* QUESTION: does this policy work well for a shared FIFO queue? *)
	     let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
	     do LockedQueue.@enqueue(readyQ, thd / exh)
	     let _ : PT.unit = SchedulerAction.@yield-in-atomic(host_vproc)
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
	let readyQ : LockedQueue.queue = LockedQueue.@new(/exh)
	let init : PT.fiber = @worker(readyQ / exh)
	fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
	    do LockedQueue.@enqueue(readyQ, thd / exh)
	    return(UNIT)
	let group : ImplicitThread.group = ImplicitThread.@group(init, spawnFn / exh)
	return(group)
      ;

    )

    val workGroup : unit -> ImplicitThread.group = _prim(@work-group)

  end
