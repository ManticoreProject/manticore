(* work-stealing-worker.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Part of the work stealing scheduler that handles stealing tasks and idling.
 *
 *)

structure WorkStealingWorker (* :
  sig

  (* workGroup () *)
  (* Spawns an instance of the work stealing scheduler. *)
  (* Each processor gets assigned one worker. *)
    val workGroup : unit -> ImplicitThread.work_group

  (* isLocalDequeEmpty () *)
  (* Returns true if the deque of the calling processor is *)
  (* empty and false otherwise. *)
    val isLocalDequeEmpty : unit -> bool

  end *) = struct

  local

    structure Arr = Array64
    structure D = WorkStealingDeque
    structure PT = PrimTypes

    _primcode (

    typedef task = ImplicitThread.thread;

  (* @get-my-deque-from-atomic (self / exh) *)
  (* Returns the deque assigned to the calling processor. *)
    define inline @get-my-deque-from-atomic (self : vproc / exh : exh) : D.deque =
	let deques : any = ImplicitThread.@get-scheduler-state (/ exh)
	do assert (NotEqual (deques, enum(0):any))
	let id : int = VProc.@vproc-id (self)
	let deque : D.deque = Arr.@sub ((Arr.array)deques, id / exh)
	do assert (NotEqual (deque, enum(0):any))
	return (deque)
      ;

  (* @set-my-deque-from-atomic (self, deques, deque / exh) *)
  (* Mark the given deque as the deque owned by the calling processor *)
  (* in the given table of deques. *)
    define inline @set-my-deque-from-atomic (self : vproc, 
					     deques : Arr.array,
					     deque : D.deque
					   / exh : exh) : () =
	let id : int = VProc.@vproc-id (self)
	do Arr.@update (deques, id, deque / exh)
	return ()
      ;

  (* @thief-from-atomic (self, victim, logWGID, logWID) *)
  (* Tries to steal tasks from the given victim vproc. Returns the list of *)
  (* stolen tasks (list is nil if steal failed). *)
  (* The thief executes on the victim vproc; the given thief vproc repeatedly *)
  (* yields until the steal attempt completes. *)
  (* pre: NotEqual(self, victim) *)
    define @thief-from-atomic (
		  self : vproc, 
		  victim : vproc, 
		  workGroupID : UID.uid, 
		  logWID : long) 
	    : (* task *) List.list =
	do assert (NotEqual (thiefVP, victimVP))
	let ch : ![(* task List.list *) Option.option] = alloc (Option.NONE)
	let ch : ![(* task List.list *) Option.option] = promote (ch)
	cont thief (_ : unit) =
	  let self : vproc = SchedulerAction.@atomic-begin ()
	  do Logging.@log-WSThiefBegin (self, tid, logWID)
	  do assert (Equal (self, victimVP))
	  let stolenTasks : List.list = @steal-threads-from-atomic (self, workGroupID, policy)
	  do case stolenTasks
	      of List.nil =>
		 let x : Option.option = promote (Option.SOME(List.nil))
		 do #0(ch) := x
		 return ()
	       | CONS (_ : task, _ : List.list) => 
		 let x : Option.option = promote (Option.SOME(stolenTasks))
		 do #0(ch) := x
		 return ()
	     end
	  do Logging.@log-WSThiefEnd (self, tid, logWID)
	  SchedulerAction.@stop ()
	let fls : FLS.fls = FLS.@get()
	do VProc.@send-from-atomic (thiefVP, victimVP, fls, thief)
	fun wait () : List.list =
	    case #0(ch)
	     of Option.NONE =>
		do Pause()
		let _ : vproc = SchedulerAction.@yield-in-atomic (thiefVP)
		apply wait ()
	      | Option.SOME (tasks : List.list) =>
		do case tasks
		    of List.nil => Logging.@log-WSThiefUnsuccessful (thiefVP, tid, logWID)
		     | _ => Logging.@log-WSThiefSuccessful (thiefVP, tid, logWID)
		   end
		return (tasks)
	    end
      (* wait for the thief to report its findings *)
	apply wait ()
      ;

  (* @try-steal-from-atomic (self, idleFlags, deque, workGroupID, logWID / exh) *)
  (* Makes a single attempt to steal a task from a processor (the  *)
  (* victim), which is chosen randomly from a uniform  *)
  (* distribution. If the victim is equal to self, then we try to *)
  (* steal from the new end of the victim's deque. Otherwise we try *)
  (* to steal from the old end. *)
    define @try-steal-from-atomic (
		  self : vproc, 
		  idleFlags : (* bool *) Arr.array,
		  deque : D.deque,
		  workGroupID : UID.uid, 
		  logWID : long
		/ exh : exh) 
	    : (* task *) List.list =
	let victimId : int = Rand.@in-range-int (0, nVProcs / exh)
	let victim : vproc = VProc.@vproc-by-id (victimId)
	if Equal (victim, self) then 
	    let t : Option.option = D.@pop-new-end-from-atomic (self, deque)
	    case t
	     of Option.NONE =>
		return (List.nil)
	      | Option.SOME (t : task) =>
		return (CONS (t, List.nil))
	    end
	else
	    let idleFlags : bool = Arr.@sub (idleFlags, victimId / exh)
	    case idleFlags
	     of true =>
		return (List.nil)    (* skip idle victim *)
	      | false =>
		@thief-from-atomic (self, victim, workGroupID, logWID)
	    end
      ;

  (* @idle-loop-from-atomic (self, setActive, idleFlags, deque, workGroupID, logWID) *)
  (* Puts the given worker in an idle state in which the worker tries to steal *)
  (* tasks. Returns a nonempty list of stolen tasks. *)
  (* To prevent busy waiting, we have the idle processor sleep for a while in *)
  (* between steal attempts. *)
    define @idle-loop-from-atomic (
		  self : vproc, 
		  setActive : fun (bool / ->),
		  idleFlags : (* bool *) Arr.array,
		  deque : D.deque,	      
		  workGroupID : UID.uid,
		  logWID : long
		/ exh : exh)
	   : (* task *) List.list =
	let waitFn : fun (/ -> bool) = SpinWait.@mk-spin-wait-fun (20)
	let nVProcs : int = VProc.@num-vprocs ()
      (* try to steal up to nVProcs times; then sleep for 1ms; repeat *)
      (* in between each steal atttempt, we execute waitFn, which spins for *)
      (* a short time *)
      (* we know to sleep when waitFn returns true; this occurs every 20th *)
      (* application of waitFn *)
	cont lp (nTries : int) =
	  if I32Gt (nTries, nVProcs) then
	      do apply setActive (false)
	      do SchedulerAction.@atomic-end (self)
	      let reset : bool = apply waitFn ()
	      do case reset
		  of true =>
		     do Logging.@log-WSSleep (self, logWID)
		     do SchedulerAction.@sleep (1000000:long)
		     return ()
		   | false =>
		 end 
	      do case terminated
		  of true => 
		     do Logging.@log-WSTerminate (self, workGroupID)
		     let _ : unit = SchedulerAction.@stop() 
		     return ()
		   | false => 
		     return ()
		 end
	      do apply setActive (true)              
	      throw findRemoteWorkLp (0)
	  else
	      let stolen : (* task *) List.list = 
		   @try-steal-from-atomic (self, idleFlags, deque, workGroupID, logWID)
	      case stolen
	       of List.nil =>
		  throw lp (I32Add (nTries, 1))
		| CONS (_ : task, _ : List.list) =>
		  return (stolen)
	      end
	throw lp (0)
      ;

#define DEFAULT_DEQUE_SZ            64

  (* @create-worker (workGroupID, logWGID, isTerminated, setActive, deques, idleFlags) *)
  (* Creates a work stealing work group; we spawn one instance (worker) per processor. *)
    define @create-worker (workGroupID : UID.uid, 
			   logWGID : long,
			   isTerminated : fun (/ -> bool), 
			   setActive : fun (bool / ->),
			   deques : (* deque *) Arr.array,
			   idleFlags : (* bool *) Arr.array
			 / exh : exh)
	    : cont (vproc, ImplicitThread.worker) =

	cont impossible () = 
	  do assert_fail()
	  throw exh (Fail(@"WorkStealing.@create-worker: impossible"))
	cont init (self : vproc, worker : ImplicitThread.worker) =
	  let deque : D.deque = D.@new-from-atomic (self, workGroupID, DEFAULT_DEQUE_SZ)
	  do @set-my-deque-from-atomic (self, deques, deque / exh)
	  let logWID : long = Logging.@log-WSWorkerInit (self, logWGID)
	  cont schedLp (self : vproc, deque : D.deque, sign : PT.signal) =
	    let vpId : int = VProc.@vproc-id (self)
	    let workerFLS : FLS.fls = FLS.@get ()
	    cont executeNextTask () = 
	      let t : Option.option = D.@pop-new-end-from-atomic (self, deque)
	      case 
	       of Option.NONE =>
		  throw schedLp (self, deque, sign)
		| Option.SOME (t : task) =>
		  do Logging.@log-WSExecute (self, logWID)
		  do @set-my-deque-from-atomic (self, deques, deque / exh)
		  do Arr.@update (idleFlags, vpId, false / exh)
		  cont a (sign : PT.signal) =
		    throw schedLp (self, deque, sign)
		  do ImplicitThread.@run-from-atomic (self, a, t / exh)
		  throw impossible ()
	      end (* executeNextTask *)
	    case sign
	     of PT.STOP =>
		let ts : (* task *) List.list = 
		   @idle-loop-from-atomic (self, setActive, idleFlags, deque, workGroupID, logWID)
		do D.@add-list-from-atomic (self, deque, ts)
		throw executeNextTask ()
	      | PT.PREEMPT (k : PT.fiber) =>
		let t : task = ImplicitThread.@capture (k / exh)
		do D.@push-new-end-from-atomic (self, deque, t)
		do Logging.@log-WSPreempted (self, wid)
		let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		throw schedLp (self, deque, PT.STOP)
	    end
	return (init)
      ;

  (* @remove-task (/ exh) *)
  (* Remove the next ready task from the queue of the host vproc. Returns *)
  (* NONE if the queue is empty and SOME t otherwise where t is the next *)
  (* ready task. *)
    define @remove-task (/ exh : exh) : (* task *) Option.option =
	let self : vproc = SchedulerAction.@atomic-begin ()
	let deque : D.deque = @get-my-deque-from-atomic (self / exh)
	let t : Option.option = D.@pop-new-end-from-atomic (self, deque)
	do SchedulerAction.@atomic-end (self)
	return (t)
      ;

  (* @spawn-task (t) *)
  (* Push task t on the new end of the deque of the host vproc. *)
    define inline @spawn-task (t : task / exh : exh) : () =
	fun lp (self : vproc) : () =
	    let deque : D.deque = @get-my-deque-from-atomic (self / exh)
	    let isFull : bool = D.@is-full (deque)
	    case isFull
	     of true =>  (* the deque is full: let the scheduler loop choose the next action *)
		PRINT_DEBUG("WorkStealing: full deque")
		throw exh (Fail(@"WorkStealing.@spawn-task: full deque"))
	      | false =>
		do D.@push-new-end-from-atomic (self, deque, t)
		do SchedulerAction.@atomic-end (self)
		return ()
	    end
	    let self : vproc = SchedulerAction.@atomic-begin ()
	apply lp (self)
      ;

    define @work-group (_ : unit / exh : exh) : ImplicitThread.work_group =
	let fls : FLS.fls = FLS.@get ()
	let workGroupID : UID.uid = UID.@new (/ exh)
	let nVProcs : int = VProc.@num-vprocs ()
      (* when #0(nIdle) = nVProcs the work, the work group is finished *)
	let nIdle : ![int] = alloc (0)
	let nIdle : ![int] = promote (nIdle)
      (* true, if the work group is finished *)
	let terminated : ![bool] = alloc (false)
	let terminated : ![bool] = promote (terminated)
      (* setActive (active) *)
      (* Changes the status of the calling worker. *)
	fun setActive (active : bool) : () =
	    case active
	     of true =>
		let _ : int = I32FetchAndAdd (&0(nIdle), ~1)
		return ()
	      | false =>
		let nAlreadyIdle : int = I32FetchAndAdd (&0(nIdle), 1)
		if I32Eq (nAlreadyIdle, I32Sub(nVProcs, 1)) then
		    do #0(terminated) := true
		    return ()
		else
		    return ()
	    end
	fun isTerminated () : bool = return (#0(terminated))
	let nVProcs : int = VProc.@num-vprocs ()
	let deques : Arr.array = Arr.@array (nVProcs, enum(0):any / exh)
	let idleFlags : Arr.array = Arr.@array (nVProcs, false / exh)
	let self : vproc = SchedulerAction.@atomic-begin ()
	let logWGID : long = Logging.@log-WSInit (self)
	do SchedulerAction.@atomic-end (self)
	let initWorker : cont (vproc, ImplicitThread.worker) = 
	   @create-worker (workGroupID, logWGID, isTerminated, setActive, deques, idleFlags / exh)
	fun spawnFn (t : task / exh : exh) : unit =
	    do @spawn-thread (t / exh)
	    return (UNIT)
	fun resumeFn (t : task / exh : exh) : unit =
	    do @resume-thread-on-new-deque (t / exh)
	    return (UNIT)
	fun removeFn (_ : task / exh : exh) : bool = 
	    @remove-thread (/ exh)
	let group : ImplicitThread.work_group = 
		    ImplicitThread.@new-work-group (workGroupID,
						    spawnFn,
						    resumeFn,
						    removeFn,
						    deques,
						    terminated
						  / exh)
	fun spawnWorker (dst : vproc / exh : exh) : () =
	    let worker : Word64.word = 
		 ImplicitThread.@spawn-worker (dst, fls, initWorker / exh)
	    return ()
	do VProc.@for-each-vproc (spawnWorker / exh)
	return (group)
      ;

    define inline @is-local-deque-empty (_ : unit / exh : exh) : bool =
	let deque : D.deque = @get-assigned-deque-from-atomic (host_vproc / exh)
	D.@is-empty (deque)
      ;

    )

  in
    val workGroup : unit -> ImplicitThread.work_group = _prim (@work-group)
    val isLocalDequeEmpty : unit -> bool = _prim (@is-local-deque-empty)
  end  (* local *)

  end
