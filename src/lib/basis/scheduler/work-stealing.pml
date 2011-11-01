(* work-stealing.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Part of the work stealing scheduler that handles stealing tasks and idling.
 *
 *)

structure WorkStealing (* :
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

  val logNumSteals = 
    List.exists (fn s => String.same (s, "-ws-log-num-steals")) 
		(CommandLine.arguments ())

  local
      val numStealsPerVProc = IntArray.array (VProc.numVProcs (), 0)
      val numFailedStealsPerVProc = IntArray.array (VProc.numVProcs (), 0)
      fun getNumSteals () = let
	val n = IntArray.length numStealsPerVProc
	fun sum i = 
	    if i < n then
		IntArray.sub (numStealsPerVProc, i) + sum (i + 1)
	    else
		0
	in
	  sum 0
	end
      fun getNumFailedSteals () = let
	val n = IntArray.length numFailedStealsPerVProc
	fun sum i = 
	    if i < n then
		IntArray.sub (numFailedStealsPerVProc, i) + sum (i + 1)
	    else
		0
	in
	  sum 0
	end
      fun incNumSteals () = let
	val id = VProc.id (VProc.host ())
	val n = IntArray.sub (numStealsPerVProc, id)
	in
	  IntArray.update (numStealsPerVProc, id, n + 1)
	end
      fun incNumFailedSteals () = let
	val id = VProc.id (VProc.host ())
	val n = IntArray.sub (numFailedStealsPerVProc, id)
	in
	  IntArray.update (numFailedStealsPerVProc, id, n + 1)
	end
  in
  val (getNumSteals, getNumFailedSteals, incNumSteals, incNumFailedSteals) =
      if logNumSteals then 
	  (getNumSteals, getNumFailedSteals, incNumSteals, incNumFailedSteals)
      else
	  (fn _ => 0, fn _ => 0, fn _ => (), fn _ => ())
  end

  local

    structure Arr = UnsafeArray
    structure D = WorkStealingDeque
    structure PT = PrimTypes

    _primcode (

    typedef task = ImplicitThread.thread;

    define @inc-num-steals = incNumSteals;
    define @inc-num-failed-steals = incNumFailedSteals;

  (* @get-my-deque-in-atomic (self / exh) *)
  (* Returns the deque assigned to the calling processor. *)
    define inline @get-my-deque-in-atomic (self : vproc / exh : exh) : D.deque =
	let deques : any = ImplicitThread.@get-scheduler-state (/ exh)
	do assert (NotEqual (deques, enum(0):any))
	let id : int = VProc.@vproc-id (self)
	let deque : D.deque = Arr.@sub ((Arr.array)deques, id / exh)
	do assert (NotEqual (deque, enum(0):any))
	return (deque)
      ;

  (* @set-my-deque-in-atomic (self, deques, deque / exh) *)
  (* Mark the given deque as the deque owned by the calling processor *)
  (* in the given table of deques. *)
    define inline @set-my-deque-in-atomic (self : vproc, 
					     deques : Arr.array,
					     deque : D.deque
					   / exh : exh) : () =
	let id : int = VProc.@vproc-id (self)
	do Arr.@update (deques, id, deque / exh)
	return ()
      ;

  (* @try-pop-remote-in-atomic (self, workGroupID) *)
  (* Try to pop a task from one of the deques of the given vproc. *)
  (* We try to pop from the new ends of the deques. *)
  (* This operation is used by the thief. *)
    define @try-pop-remote-in-atomic (
		      self : vproc,
		      workGroupID : UID.uid)
		 : (* task *) List.list =
	cont succeed (ts : (* task *) List.list) = return (ts)
      (* try resume deques *)
	let deques : (* D.deque *) List.list = 
		D.@resume-deques-in-atomic (self, workGroupID)
	do case deques
	    of List.nil =>
	       return ()
	     | CONS (d : [D.deque], _ : List.list) =>
	       let stolenTask : Option.option = D.@pop-old-end-in-atomic (self, #0(d))
	       case stolenTask
		of Option.NONE =>
		   return ()
		 | Option.SOME (t : task) =>
		   throw succeed (CONS (t, List.nil))
	       end
	   end
      (* try primary deque *)
	let deque : Option.option = D.@primary-deque-in-atomic (self, workGroupID)
        do case deque
	 of Option.NONE =>
	    return ()
	  | Option.SOME (deque : D.deque) =>
	    let n : int = D.@size (deque)
            if I32Gt (n, 1) then
		let stolenTask : Option.option = D.@pop-old-end-in-atomic (self, deque)
	        case stolenTask
		 of Option.NONE =>
		    return ()
		  | Option.SOME (t : task) =>
		    throw succeed (CONS (t, List.nil))
	        end
	    else
		return ()
	end
      (* try secondary deque *)
	let deque : Option.option = D.@secondary-deque-in-atomic (self, workGroupID)
        do case deque
	 of Option.NONE =>
	    return ()
	  | Option.SOME (deque : D.deque) =>
	    let stolenTask : Option.option = D.@pop-old-end-in-atomic (self, deque)
	    case stolenTask
	      of Option.NONE =>
		 return ()
	       | Option.SOME (t : task) =>
		 throw succeed (CONS (t, List.nil))
	    end
        end
	return (List.nil)
      ;

  (* @try-pop-local-in-atomic (self, workGroupID) *)
  (* Try to pop a task from one of the deques of the given vproc. *)
  (* We try to pop from the new ends of the deques. *)
  (* This operation is used by the host. *)
    define @try-pop-local-in-atomic (
		      self : vproc,
		      workGroupID : UID.uid)
		 : (* task *) List.list =
	cont succeed (ts : (* task *) List.list) = return (ts)
      (* try resume deques *)
	let deques : (* D.deque *) List.list = 
		D.@resume-deques-in-atomic (self, workGroupID)
	do case deques
	    of List.nil =>
	       return ()
	     | CONS (d : [D.deque], _ : List.list) =>
	       let stolenTask : Option.option = D.@pop-new-end-in-atomic (self, #0(d))
	       case stolenTask
		of Option.NONE =>
		   return ()
		 | Option.SOME (t : task) =>
		   throw succeed (CONS (t, List.nil))
	       end
	   end
      (* try primary deque *)
	let deque : Option.option = D.@primary-deque-in-atomic (self, workGroupID)
        do case deque
	 of Option.NONE =>
	    return ()
	  | Option.SOME (deque : D.deque) =>
	    let stolenTask : Option.option = D.@pop-new-end-in-atomic (self, deque)
	    case stolenTask
	      of Option.NONE =>
		 return ()
	       | Option.SOME (t : task) =>
		 throw succeed (CONS (t, List.nil))
	     end
        end
      (* try secondary deque *)
	let deque : Option.option = D.@secondary-deque-in-atomic (self, workGroupID)
        do case deque
          of Option.NONE =>
	     return ()
	   | Option.SOME (deque : D.deque) =>
	     let stolenTask : Option.option = D.@pop-new-end-in-atomic (self, deque)
	     case stolenTask
	       of Option.NONE =>
		  return ()
		| Option.SOME (t : task) =>
		  throw succeed (CONS (t, List.nil))
	     end
	end
	return (List.nil)
      ;

  (* @thief-in-atomic (self, victim, logWGID, logWID / exh) *)
  (* Tries to steal tasks from the given victim vproc. Returns the list of *)
  (* stolen tasks (list is nil if steal failed). *)
  (* The thief executes on the victim vproc; the given thief vproc repeatedly *)
  (* yields until the steal attempt completes. *)
  (* pre: NotEqual(self, victim) *)
    define @thief-in-atomic (
		  self : vproc, 
		  victim : vproc, 
		  workGroupID : UID.uid, 
		  logWID : long / exh : exh) 
	    : (* task *) List.list =
	do assert (NotEqual (self, victim))
        let logTID : long = Logging.@log-WSThiefSend (self, logWID)
	let ch : ![(* task List.list *) Option.option] = alloc (Option.NONE)
	let ch : ![(* task List.list *) Option.option] = promote (ch)
	cont thief (_ : unit) =
	  let self : vproc = SchedulerAction.@atomic-begin ()
	  do Logging.@log-WSThiefBegin (self, logTID, logWID)
	  do assert (Equal (self, victim))
          let ts : (* task *) List.list = @try-pop-remote-in-atomic (self, workGroupID)
          case ts
	   of List.nil =>
	      let x : Option.option = promote (Option.SOME(List.nil))
	      do #0(ch) := x
	      do Logging.@log-WSThiefEnd (self, logTID, logWID)
	      SchedulerAction.@stop ()
	    | CONS (_ : task, _ : List.list) =>
	      let x : Option.option = promote (Option.SOME(ts))
	      do #0(ch) := x
	      do Logging.@log-WSThiefEnd (self, logTID, logWID)
	      SchedulerAction.@stop ()
	  end
	let fls : FLS.fls = FLS.@get()
	do VProc.@send-in-atomic (self, victim, fls, thief)
	fun wait () : List.list =
	    case #0(ch)
	     of Option.NONE =>
		do Pause()
		let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		apply wait ()
	      | Option.SOME (tasks : List.list) =>
		do case tasks
		    of List.nil => 
		       let _ : unit = @inc-num-failed-steals (UNIT / exh)
		       Logging.@log-WSThiefUnsuccessful (self, logTID, logWID)
		     | _ => 
		       let _ : unit = @inc-num-steals (UNIT / exh)
		       Logging.@log-WSThiefSuccessful (self, logTID, logWID)
		   end
		return (tasks)
	    end
	apply wait ()
      ;

  (* @try-steal-in-atomic (self, idleFlags, deque, workGroupID, logWID / exh) *)
  (* Makes a single attempt to steal a task from a processor (the  *)
  (* victim), which is chosen randomly from a uniform  *)
  (* distribution. If the victim is equal to self, then we try to *)
  (* steal from the new end of the victim's deque. Otherwise we try *)
  (* to steal from the old end. *)
    define @try-steal-in-atomic (
		  self : vproc, 
		  idleFlags : (* bool *) Arr.array,
		  deque : D.deque,
		  workGroupID : UID.uid, 
		  logWID : long
		/ exh : exh) 
	    : (* task *) List.list =
	let nVProcs : int = VProc.@num-vprocs ()
	let victimId : int = Rand.@in-range-int (0, nVProcs / exh)
	let victim : vproc = VProc.@vproc-by-id (victimId)
	if Equal (victim, self) then
	    @try-pop-local-in-atomic (self, workGroupID)
	else
	    let idleFlags : bool = Arr.@sub (idleFlags, victimId / exh)
	    case idleFlags
	     of true =>
		return (List.nil)    (* skip idle victim *)
	      | false =>
		@thief-in-atomic (self, victim, workGroupID, logWID / exh)
	    end
      ;

  (* @idle-loop-in-atomic (self, setActive, isTerminated, idleFlags, deque, workGroupID, logWID) *)
  (* Puts the given worker in an idle state in which the worker tries to steal *)
  (* tasks. Returns a nonempty list of stolen tasks. *)
  (* To prevent busy waiting, we have the idle processor sleep for a while in *)
  (* between steal attempts. *)
    define @idle-loop-in-atomic (
		  self : vproc, 
		  setActive : fun (bool / ->),
	          isTerminated : fun (/ -> bool), 
		  idleFlags : (* bool *) Arr.array,
		  deque : D.deque,	      
		  workGroupID : UID.uid,
		  logWID : long
		/ exh : exh)
	   : (* task *) List.list =
	let waitFn : fun (/ -> bool) = SpinWait.@mk-spin-wait-fun (20)
	let nVProcs : int = VProc.@num-vprocs ()
	let vpId : int = VProc.@vproc-id (self)
      (* try to steal up to nVProcs times; then sleep for 1ms; repeat *)
      (* in between each steal atttempt, we execute waitFn, which spins for *)
      (* a short time. *)
      (* we know to sleep when waitFn returns true; this occurs every 20th *)
      (* application of waitFn. *)
	cont lp (nTries : int) =
	  if I32Gt (nTries, nVProcs) then
	      do SchedulerAction.@atomic-end (self)
	      do apply setActive (false)
	      do Arr.@update (idleFlags, vpId, true / exh)
	      let reset : bool = apply waitFn ()
	      do case reset
		  of true =>
		     do Logging.@log-WSSleep (self, logWID)
		     do SchedulerAction.@sleep (1000000:long)
		     return ()
		   | false =>
		     return ()
		 end 
              let terminated : bool = apply isTerminated ()
	      do case terminated
		  of true => 
		     do Logging.@log-WSTerminate (self, workGroupID)
		     let _ : unit = SchedulerAction.@stop() 
		     return ()
		   | false => 
		     return ()
		 end
              do Arr.@update (idleFlags, vpId, false / exh)
	      do apply setActive (true)
	      let _ : vproc = SchedulerAction.@atomic-begin ()
	      throw lp (0)
	  else	      
              let _ : vproc = SchedulerAction.@yield-in-atomic (self)
	      let stolen : (* task *) List.list = 
		   @try-steal-in-atomic (self, idleFlags, deque, workGroupID, logWID / exh)
	      case stolen
	       of List.nil =>
		  throw lp (I32Add (nTries, 1))
		| CONS (t : task, l : List.list) =>                  
		  return (stolen)
	      end
	throw lp (0)
      ;

#define DEFAULT_DEQUE_SZ            64000

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
	  throw exh (Fail(@"WorkStealing.@create-worker: impossible"))
	cont init (self : vproc, worker : ImplicitThread.worker) =
	  let deque : D.deque = 
                D.@new-primary-deque-in-atomic (self, workGroupID, DEFAULT_DEQUE_SZ)
          let deque2 : D.deque = 
                D.@new-secondary-deque-in-atomic (self, workGroupID, DEFAULT_DEQUE_SZ)
	  do @set-my-deque-in-atomic (self, deques, deque / exh)
	  let logWID : long = Logging.@log-WSWorkerInit (self, logWGID)
	  cont schedLp (sign : PT.signal) =
	    let workerFLS : FLS.fls = FLS.@get ()
	    cont executeNextTask () = 
	      let t : Option.option = D.@pop-new-end-in-atomic (self, deque)
	      case t
	       of Option.NONE =>
                   let ts : (* task *) List.list = @try-pop-local-in-atomic (self, workGroupID)
                   do case ts
		    of List.nil =>
                       let ts : (* task *) List.list = 
		           @idle-loop-in-atomic (self, setActive, isTerminated, idleFlags, deque, workGroupID, logWID / exh)
                       D.@add-list-in-atomic (self, deque, ts)
		     | CONS (_ : task, _ : List.list) =>
		       D.@add-list-in-atomic (self, deque, ts)
                   end
                   throw executeNextTask ()
		| Option.SOME (t : task) =>
		  do Logging.@log-WSExecute (self, logWID)
		  do @set-my-deque-in-atomic (self, deques, deque / exh)
		  do ImplicitThread.@run-from-atomic (self, schedLp, t / exh)
		  throw impossible ()
	      end (* executeNextTask *)
	    case sign
	     of PT.STOP =>
		throw executeNextTask ()
	      | PT.PREEMPT (k : PT.fiber) =>
		do Logging.@log-WSPreempted (self, logWID)
		let t : task = ImplicitThread.@capture (k / exh)
		do D.@push-new-end-in-atomic (self, deque, t)
		let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		throw executeNextTask ()
	      | PT.BLOCK (k : PT.fiber) =>
		do Logging.@log-WSPreempted (self, logWID)
		let t : task = ImplicitThread.@capture (k / exh)
		do D.@push-new-end-in-atomic (self, deque, t)
		let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		throw executeNextTask ()
	    end
          throw schedLp (PT.STOP)
	return (init)
      ;

  (* @remove-task (/ exh) *)
  (* Remove the next ready task from the queue of the host vproc. Returns *)
  (* NONE if the queue is empty and SOME t otherwise where t is the next *)
  (* ready task. *)
    define @remove-task (/ exh : exh) : (* task *) Option.option =
	let self : vproc = SchedulerAction.@atomic-begin ()
	let deque : D.deque = @get-my-deque-in-atomic (self / exh)
	let t : Option.option = D.@pop-new-end-in-atomic (self, deque)
	do SchedulerAction.@atomic-end (self)
	return (t)
      ;

  (* @spawn-task (t) *)
  (* Push task t on the new end of the deque of the host vproc. *)
    define inline @spawn-task (t : task / exh : exh) : () =
	fun lp (self : vproc) : () =
	    let deque : D.deque = @get-my-deque-in-atomic (self / exh)
	    let isFull : bool = D.@is-full (deque)
	    case isFull
	     of true =>  (* the deque is full: let the scheduler loop choose the next action *)
		do ccall M_Print ("WorkStealing: full deque\n")
		throw exh (Fail(@"WorkStealing.@spawn-task: full deque"))
	      | false =>
		do D.@push-new-end-in-atomic (self, deque, t)
		do SchedulerAction.@atomic-end (self)
		return ()
	    end
	let self : vproc = SchedulerAction.@atomic-begin ()
	apply lp (self)
      ;

  (* @resume-task-on-new-deque (t / exh *)
  (* resumes task t on a new deque *)
    define inline @resume-task-on-new-deque (t : task / exh : exh) : () =
	let self : vproc = SchedulerAction.@atomic-begin ()
	let workGroup : ImplicitThread.work_group = ImplicitThread.@current-work-group (UNIT / exh)
	let workGroupId : UID.uid = ImplicitThread.@work-group-id (workGroup)
	let deque : D.deque = D.@new-resume-deque-in-atomic (self, workGroupId, DEFAULT_DEQUE_SZ)
	do D.@push-new-end-in-atomic (self, deque, t)
	do D.@release-in-atomic (self, deque)
	do SchedulerAction.@atomic-end (self)
	return ()
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
	let deques : Arr.array = Arr.@create (nVProcs, enum(0):any / exh)
	let idleFlags : Arr.array = Arr.@create (nVProcs, false / exh)
	let self : vproc = SchedulerAction.@atomic-begin ()
	let logWGID : long = Logging.@log-WSInit (self)
	do SchedulerAction.@atomic-end (self)
	let initWorker : cont (vproc, ImplicitThread.worker) = 
	   @create-worker (workGroupID, logWGID, isTerminated, setActive, deques, idleFlags / exh)
	fun spawnFn (t : task / exh : exh) : unit =
	    do @spawn-task (t / exh)
	    return (UNIT)
	fun resumeFn (t : task / exh : exh) : unit =
	    do @resume-task-on-new-deque (t / exh)
	    return (UNIT)
	fun removeFn (_ : task / exh : exh) : Option.option = 
	    @remove-task (/ exh)
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
	let deque : D.deque = @get-my-deque-in-atomic (host_vproc / exh)
	D.@is-empty (deque)
      ;

    )

  in
    val workGroup : unit -> ImplicitThread.work_group = _prim (@work-group)
    val isLocalDequeEmpty : unit -> bool = _prim (@is-local-deque-empty)
  end  (* local *)

  end
