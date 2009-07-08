(* work-stealing.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The Work Stealing scheduler.
 *)

structure WorkStealing (* :
  sig

    val workGroup : unit -> ImplicitThread.work_group

  end *) = struct

    local

      structure Arr = Array64
      structure D = WorkStealingDeque
      structure PT = PrimTypes

      _primcode (
        
      (* returns the assigned deque for the host vproc *)
	define (* inline *) @get-assigned-deque-from-atomic (self : vproc / exh : exh) : D.deque =
	    let deques : any = ImplicitThread.@get-scheduler-state (/ exh)
            do assert (NotEqual (deques, enum(0):any))
	    let id : int = VProc.@vproc-id (self)
	    let deque : D.deque = Arr.@sub ((Arr.array)deques, id / exh)
	    do assert (NotEqual (deque, enum(0):any))
 	    return (deque)
	  ;
        
      (* set the assigned deque for the host vproc *)
	define (* inline *) @set-assigned-deque-from-atomic (self : vproc, 
							     assignedDeques : Arr.array,
							     deque : D.deque / exh : exh) : () =
	    let id : int = VProc.@vproc-id (self)
	    do Arr.@update (assignedDeques, id, deque / exh)
	    return ()
	  ;

	define @mug-from-atomic (self : vproc, workGroupId : UID.uid) : (* ImplicitThread.thread *) List.list =
	  (* a deque is muggable when it is neither claimed nor empty *)
	    fun isMuggable (deque : [D.deque] / exh : exh) : bool =
		let deque : D.deque = #0(deque)
		let isClaimed : bool = D.@is-claimed-from-atomic (self, deque)
		let isNotClaimed : bool = PrimBool.@not (isClaimed)
		let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
		let isNotEmpty : bool = PrimBool.@not (isEmpty)
		PrimBool.@and (isNotClaimed, isNotEmpty)
	    let localDeques : List.list = D.@local-deques-from-atomic (self, workGroupId)
            cont exh (_ : exn) = return (List.nil)
	    let muggable : List.list = PrimList.@filter (isMuggable, localDeques / exh)
	    let threads : List.list = case muggable
				       of List.nil => 					  
					  return (List.nil)
					| List.CONS (deque : [D.deque], _ : List.list) => 
					  D.@to-list-from-atomic (self, #0(deque))
	                              end
            do D.@release-deques-from-atomic (self, localDeques)
            return (threads)
	  ;

      (* returns a thread taken from the "old" end of one of the deques local to the given vproc, if such a
       * thread exists *)
	define @steal-thread-from-atomic (self : vproc, workGroupId : UID.uid) : (* ImplicitThread.thread *) Option.option =
	    fun isNonEmpty (deque : [D.deque] / exh : exh) : bool =
		let deque : D.deque = #0(deque)
		let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
		PrimBool.@not (isEmpty)
	    let localDeques : List.list = D.@local-deques-from-atomic (self, workGroupId)
            cont exh (_ : exn) = return (Option.NONE)
	    let nonEmptyDeques : List.list = PrimList.@filter (isNonEmpty, localDeques / exh)
	    let thd : Option.option = case nonEmptyDeques
				       of List.nil =>
					  return (Option.NONE)
					| List.CONS (deque : [D.deque], _ : List.list) =>
					  D.@pop-old-end-from-atomic (self, #0(deque))
				      end
	    do D.@release-deques-from-atomic (self, localDeques)
	    return (thd)
	  ;

      (* send a thief from thiefVP to steal from victimVP. the result list is the list of stolen threads. *)
	define @thief-from-atomic (thiefVP : vproc, victimVP : vproc, workGroupId : UID.uid)
								      : (* ImplicitThread.thread *) List.list =
	    do assert (NotEqual (thiefVP, victimVP))
	  (* communication channel used to pass the result of the steal from the victim vproc back to the
	   * originating vproc thiefVP *)
	    let ch : ![Option.option] = alloc (Option.NONE)
	    let ch : ![Option.option] = promote (ch)
	  (* the thief fiber executes on victimVP *)
	    cont thief (_ : unit) =
	      let self : vproc = SchedulerAction.@atomic-begin ()
	      do assert (Equal (self, victimVP))
	    (* try to mug first *)
	      let muggedThreads : List.list = @mug-from-atomic (self, workGroupId)
	      do case muggedThreads
		  of List.nil => 
		     return ()
		   | List.CONS (_ : ImplicitThread.thread, _ : List.list) =>
		     let x : Option.option = promote (Option.SOME(muggedThreads))
		     do #0(ch) := x
		     SchedulerAction.@stop ()
		 end
	    (* unsuccessful mugging attempt, try to steal *)
	      let stolenThread : Option.option = @steal-thread-from-atomic (self, workGroupId)
	      do case stolenThread
		  of Option.NONE => (* unsuccessful steal attempt *)
		     let x : Option.option = promote (Option.SOME(List.nil))
		     do #0(ch) := x
		     return ()
		   | Option.SOME (thd : ImplicitThread.thread) => (* successful: stole thd *)
		     let x : Option.option = promote (Option.SOME(List.CONS (thd, nil)))
		     do #0(ch) := x
		     return ()
		 end
	      SchedulerAction.@stop ()
	  (* send the thief fiber to the victim vproc *)
	    do VProc.@send-high-priority-signal-from-atomic (thiefVP, victimVP, thief)
	    fun wait () : List.list =
		case #0(ch)
		 of Option.NONE =>
		    do Pause()
		    let _ : vproc = SchedulerAction.@yield-in-atomic (thiefVP)
		    apply wait ()
		  | Option.SOME (thds : List.list) =>
		    return (thds)
		end
	  (* wait for the thief to report its findings *)
	    apply wait ()
	  ;

      (* loop until at least one ready thread has been stolen; the result is the list of stolen threads *)
	define @find-work-in-atomic (self : vproc, 
				     workGroupId : UID.uid,
				     pickVictim : fun (vproc / -> (* [vproc] *) Option.option))
					                    : (* ImplicitThread.thread *) List.list =
          (* try to mug a worker local to this vproc *)
	    let muggedThreads : List.list = @mug-from-atomic (self, workGroupId)
	    case muggedThreads
	     of List.CONS (_ : ImplicitThread.thread, _ : List.list) =>
		(* successful mugging *)
		return (muggedThreads)
	      | List.nil =>
		(* mugging failed; try to steal *)
		let victimVP : Option.option = apply pickVictim (self)
                case victimVP
		 of Option.NONE =>
		    (* no victims currently available *)
		    return (List.nil)
		  | Option.SOME (victimVP : [vproc]) =>
		    @thief-from-atomic (self, #0(victimVP), workGroupId)
                end		    
	    end
	  ;

	define @new-worker (workGroupId : UID.uid, 
			    isTerminated : fun (/ -> bool), 
  		            assignedDeques : Arr.array,
                            pickVictim : fun (vproc / -> (* [vproc] *) Option.option)
			  / exh : exh) 
	                                         : cont (vproc, ImplicitThread.worker) =
	    cont impossible () = 
	      do assert_fail()
	      throw exh (Fail(@"WorkStealing.@designated-worker: impossible"))
	    cont schedulerLoop (self : vproc, worker : ImplicitThread.worker, deque : D.deque, sign : PT.signal) =
	      cont dispatch (thd : ImplicitThread.thread) = 
		cont act (sign : PT.signal) = throw schedulerLoop (self, worker, deque, sign)
		do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
		do ImplicitThread.@run-from-atomic (self, act, thd / exh)
		throw impossible ()
	      let isFinished : bool = apply isTerminated ()
	      do case isFinished
		  of true =>
		     let _ : List.list = D.@to-list-from-atomic (self, deque) (* empty the deque *)
		     do D.@release-from-atomic (self, deque)
		     do SchedulerAction.@stop-from-atomic (self)
		     throw impossible ()
		   | false =>
		     return ()
		 end
	      case sign
	       of PT.STOP =>
		  let thd : Option.option = D.@pop-new-end-from-atomic (self, deque)
		  case thd
		   of Option.NONE =>
		      let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		      let stolenThds : List.list = @find-work-in-atomic (self, workGroupId, pickVictim)
                      do D.@add-list-from-atomic (self, deque, stolenThds)
		      throw schedulerLoop (self, worker, deque, PT.STOP)
		    | Option.SOME (thd : ImplicitThread.thread) =>
		      throw dispatch (thd)
		  end
		| PT.PREEMPT (k : PT.fiber) =>
		  let isFull : bool = D.@is-full (deque)
		  case isFull
		   of true => (* the deque is full: resize the deque and then preempt the thread *)
		      let newDeque : D.deque = D.@double-size-from-atomic (self, workGroupId, deque)
                      throw schedulerLoop (self, worker, newDeque, sign)
		    | false =>
		      let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		      do D.@push-new-end-from-atomic (self, deque, thd)  (* make the thread available to thieves *)
		      let _ : vproc = SchedulerAction.@yield-in-atomic (self)
                      throw schedulerLoop (self, worker, deque, PT.STOP)
		 end
	       | _ =>
		  throw impossible ()
	      end (* schedulerLoop *)
	    cont initWorker (self : vproc, worker : ImplicitThread.worker) =	      
	      let deque : D.deque = D.@new-from-atomic (self, workGroupId, 128)
	      do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
	      throw schedulerLoop (self, worker, deque, PT.STOP)
	    return (initWorker)
	  ;

      (* return true, if there was an element on the deque *)
	define (* inline *) @remove-thread (/ exh : exh) : bool =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    let deque : D.deque = @get-assigned-deque-from-atomic (self / exh)
	    let k : Option.option = D.@pop-new-end-from-atomic (self, deque)
	    do SchedulerAction.@atomic-end (self)
	    case k
	     of Option.NONE =>
		return (false)
	      | Option.SOME (k : ImplicitThread.thread) =>
		return (true)
	    end
	  ;

	define (* inline *) @spawn-thread (thd : ImplicitThread.thread / exh : exh) : () =
	    fun lp (self : vproc) : () =
		let deque : D.deque = @get-assigned-deque-from-atomic (self / exh)
		let isFull : bool = D.@is-full (deque)
		case isFull
		 of true =>  (* the deque is full: let the scheduler loop choose the next action *)
		    let self : vproc = SchedulerAction.@yield-in-atomic (self)
		    apply lp (self)
		  | false =>
		    do D.@push-new-end-from-atomic (self, deque, thd)
		    do SchedulerAction.@atomic-end (self)
		    return ()
		end
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    apply lp (self)
	  ;

      (* one of several policies for resuming a thread. here we create a deque containing just the given thread. *)
	define (* inline *) @resume-thread-on-new-deque (thd : ImplicitThread.thread / exh : exh) : () =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    let workGroup : ImplicitThread.work_group = ImplicitThread.@current-work-group (UNIT / exh)
	    let workGroupId : UID.uid = ImplicitThread.@work-group-id (workGroup)
	    let deque : D.deque = D.@new-from-atomic (self, workGroupId, 1)
	    do D.@push-new-end-from-atomic (self, deque, thd)
	    do D.@release-from-atomic (self, deque)
	    do SchedulerAction.@atomic-end (self)
	    return ()
	  ;

	define @work-group (_ : unit / exh : exh) : ImplicitThread.work_group =
	    let fls : FLS.fls = FLS.@get ()
	    let uid : UID.uid = UID.@new (/ exh)
	    let terminated : ![bool] = ImplicitThread.@terminated-flag ()
	    fun isTerminated () : bool = return (#0(terminated))
	    fun pickVictim (self : vproc) : (* [vproc] *) Option.option =
		let nVProcs : int = VProc.@num-vprocs ()
		if I32Lte (nVProcs, 1) then
		    return (Option.NONE)
		else
		    let victimVPId : int = Rand.@in-range-int (0, nVProcs / exh)
		    let victimVP : vproc = VProc.@vproc-by-id (victimVPId)
		    if Equal (victimVP, self) then 
			(* never steal from the host vproc *)
			apply pickVictim (self)
		    else
			return (Option.SOME (alloc (victimVP)))
	    let nVProcs : int = VProc.@num-vprocs ()
	    let assignedDeques : Arr.array = Arr.@array (nVProcs, enum(0):any / exh)
	    let initWorker : cont (vproc, ImplicitThread.worker) = 
			     @new-worker (uid, isTerminated, assignedDeques, pickVictim / exh)
	    fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
		do @spawn-thread (thd / exh)
		return (UNIT)
	    fun resumeFn (thd : ImplicitThread.thread / exh : exh) : unit =
		do @resume-thread-on-new-deque (thd / exh)
		return (UNIT)
	    fun removeFn (_ : ImplicitThread.thread / exh : exh) : bool = 
		@remove-thread (/ exh)
	    let group : ImplicitThread.work_group = 
			ImplicitThread.@new-work-group (uid,
							spawnFn,
							resumeFn,
							removeFn,
							assignedDeques,
							terminated
						      / exh)
	    fun spawnWorker (dst : vproc / exh : exh) : () =
		let worker : Word64.word = ImplicitThread.@spawn-worker (group, dst, fls, initWorker / exh)
		return ()
	    do VProc.@for-each-vproc (spawnWorker / exh)
	    return (group)
	  ;

      )

    in

    val workGroup : unit -> ImplicitThread.work_group = _prim (@work-group)

    end

  end
