(* global-bfs-scheduler.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Scheduling policy for implicit threads:
 *   - makes a bread-first traversal of the spawn tree
 *   - uses a global FIFO scheduling queue
 *)

structure GlobalBFSScheduler (* : 
  sig 
    val workGroup : unit -> ImplicitThread.work_group
  end *) = struct

    structure PT = PrimTypes

    _primcode (

      define @designated-worker (readyQ : LockedQueue.queue / exh : exh) : PT.fiber =
	  cont schedulerLoop (s : PT.signal) =
	    let self : vproc = host_vproc

	    cont dispatch () =
	      do SchedulerAction.@yield-in-atomic (self)
	      let thd : Option.option = LockedQueue.@dequeue-from-atomic (readyQ)
	      case thd
	       of Option.NONE =>
		  throw dispatch ()
		| Option.SOME(thd : ImplicitThread.thread) =>
		  do ImplicitThread.@run-from-atomic (self, schedulerLoop, thd / exh)
		  throw dispatch ()
	      end
	   case s
	    of PT.STOP =>
	       throw dispatch ()
	     | PT.PREEMPT(k : PT.fiber) =>
	       let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
               do SchedulerAction.@yield-in-atomic (self)
	       do ImplicitThread.@run-from-atomic (self, schedulerLoop, thd / exh)
	       let e : exn = Match
	       throw exh (e)
	     | _ => 
	       let e : exn = Match
	       throw exh (e)
	   end

	  cont initK (x : unit) =
	    throw schedulerLoop (PT.STOP)

	  return (initK)
	;

    define @work-group (_ : unit / exh : exh) : ImplicitThread.work_group =
	let readyQ : LockedQueue.queue = LockedQueue.@new ()
        let uid : UID.uid = UID.@new (/ exh)
        let terminated : ![bool] = ImplicitThread.@terminated-flag ()
	let designatedWorkerInit : PT.fiber = @designated-worker (readyQ / exh)
        cont auxiliaryWorkerInit (_ : unit) = 
          let e : exn = Fail(@"GlobalBFSScheduler.@work-group: todo: implement auxiliary worker")
          throw exh (e)
	fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
            let vp : vproc = SchedulerAction.@atomic-begin ()
	    do LockedQueue.@enqueue-from-atomic (readyQ, thd)
            do SchedulerAction.@atomic-end (vp)
	    return (UNIT)
        fun removeFn (thd : ImplicitThread.thread / exh : exh) : bool = return (false)
	let group : ImplicitThread.work_group = 
		ImplicitThread.@new-work-group (uid,
						designatedWorkerInit,
						auxiliaryWorkerInit,
						spawnFn,
						removeFn,
						enum(0):any,
						terminated
					      / exh)
	return (group)
      ;

    )

    val workGroup : unit -> ImplicitThread.work_group = _prim(@work-group)

  end
