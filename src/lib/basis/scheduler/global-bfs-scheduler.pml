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

      define @new-worker (readyQ : LockedQueue.queue / exh : exh) : cont (vproc, ImplicitThread.worker) =
	  cont initWorker (self : vproc, worker : ImplicitThread.worker) =
	    let waitFn : fun (/ -> bool) = SpinWait.@mk-spin-wait-fun (15)
	    cont schedulerLoop (self : vproc, s : PT.signal) =
	      cont run (thd : ImplicitThread.thread) = 
		cont act (s : PT.signal) = throw schedulerLoop (self, s)
		do ImplicitThread.@run-from-atomic (self, act, thd / exh)
		throw exh (Match)

	      cont dispatch () =
		let thd : Option.option = LockedQueue.@dequeue-from-atomic (readyQ)
		case thd
		 of Option.NONE =>
		    let _ : vproc = SchedulerAction.@yield-in-atomic (self)
                    do SchedulerAction.@atomic-end (self)
		    let reset : bool = apply waitFn ()
                    do case reset
			of true =>
			   do SchedulerAction.@sleep (1000000:long)
                           return ()
			 | false =>
			   return()
                       end
                    let _ : vproc = SchedulerAction.@atomic-begin ()
		    throw dispatch ()
		  | Option.SOME(thd : ImplicitThread.thread) =>
		    throw run (thd)
		end

	     case s
	      of PT.STOP =>
		 throw dispatch ()
	       | PT.PREEMPT(k : PT.fiber) =>
		 let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		 let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		 throw run (thd)
	       | PT.BLOCK (k : PT.fiber) =>
		 let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		 let _ : vproc = SchedulerAction.@block-in-atomic (self)
		 throw run (thd)
	       | _ => 
		 throw exh (Match)
	     end

	    throw schedulerLoop (self, PT.STOP)

	  return (initWorker)
	;

    define @work-group (_ : unit / exh : exh) : ImplicitThread.work_group =
        let fls : FLS.fls = FLS.@get ()
	let readyQ : LockedQueue.queue = LockedQueue.@new ()
        let uid : UID.uid = UID.@new (/ exh)
        let terminated : ![bool] = ImplicitThread.@terminated-flag ()
	let initWorker : cont (vproc, ImplicitThread.worker) = @new-worker (readyQ / exh)
	fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
            let vp : vproc = SchedulerAction.@atomic-begin ()
	    do LockedQueue.@enqueue-from-atomic (readyQ, thd)
            do SchedulerAction.@atomic-end (vp)
	    return (UNIT)
        fun removeFn (thd : ImplicitThread.thread / exh : exh) : Option.option = return (Option.NONE)
	let group : ImplicitThread.work_group = 
		ImplicitThread.@new-work-group (uid,
						spawnFn,
			      (* in this policy there is no difference between spawning and resuming a thread *)
						spawnFn,
						removeFn,
						enum(0):any,
						terminated
					      / exh)
	fun spawnWorker (dst : vproc / exh : exh) : () =
	    let worker : Word64.word = ImplicitThread.@spawn-worker (dst, fls, initWorker / exh)
	    return ()
	do VProc.@for-each-vproc (spawnWorker / exh)
	return (group)
      ;

    )

    val workGroup : unit -> ImplicitThread.work_group = _prim(@work-group)

  end
