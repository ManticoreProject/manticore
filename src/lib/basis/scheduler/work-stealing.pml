(* work-stealing.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The Work Stealing scheduler.
 *)

structure WorkStealing (* :
  sig

    _prim (

      define inline @push-new-end (thd : ImplicitThread.thread) : ();
    (* return true, if there was an element on the deque *)
      define inline @pop-new-end () : bool;

    )

    val workGroup : unit -> ImplicitThread.work_group

  end *) = struct

    local

      structure Arr = Array64
      structure D = WorkStealingDeque
      structure PT = PrimTypes

      _primcode (

#define INITIAL_DEQUE_SIZE   128

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

      (* returns threads stolen from the victim vproc *)
      (* precondition: NotEqual (self, victim) *)
	define @steal-from-atomic (self : vproc, victim : vproc, workGroupId : UID.uid) : List.list =
	    do assert (NotEqual (self, victim))

	    let res : ![Option.option] = alloc (Option.NONE)
	    let res : ![Option.option] = promote (res)

	    cont thief (_ : unit) =
	      let self : vproc = SchedulerAction.@atomic-begin ()
	      let deques : List.list = D.@local-deques-from-atomic (self, workGroupId)
	      fun selectNonEmptyDeque (deques : List.list) : Option.option =
		  case deques
		   of nil => 
		      return (Option.NONE)
		    | List.CONS (deque : [D.deque], deques : List.list) =>
		      let deque : D.deque = #0(deque)
		      let isEmpty : bool = D.@is-empty-from-atomic (self, deque)
		      if isEmpty then
			  apply selectNonEmptyDeque (deques)
		      else
			  return (Option.SOME(deque))
		  end
	      let deque : Option.option = apply selectNonEmptyDeque (deques)
	      do case deque
		  of Option.NONE =>
		   (* found no available deques *)
		     let x : Option.option = promote (Option.SOME(List.nil))
		     do #0(res) := x
		     return ()
		   | Option.SOME (deque : D.deque) =>
		     let thd : Option.option = D.@pop-old-end-from-atomic (self, deque)
		     case thd
		      of Option.NONE => 
		       (* the deque returned by selectNonEmptyDeque should not be empty *)			 
			 do assert (false)
			 return ()
		       | Option.SOME (thd : ImplicitThread.thread) =>
			 let x : Option.option = promote (Option.SOME(List.CONS (thd, nil)))
		         do #0(res) := x
			 return ()
		     end
	         end
	      do D.@release-deques-from-atomic (self, deques)
              do SchedulerAction.@atomic-end (self)
	      SchedulerAction.@stop ()

          (* send the thief fiber to the victim vproc *)
            do VProc.@send-high-priority-signal-from-atomic (self, victim, thief)

	    fun wait () : List.list =
		case #0(res)
		 of Option.NONE =>
		    do Pause()
		    do SchedulerAction.@yield-in-atomic (self)
		    apply wait ()
		  | Option.SOME (thds : List.list) =>
		    return (thds)
		end

	  (* wait for the thief to report its findings *)
	    let xs : List.list = apply wait ()
            return (xs)
	  ;

    (* push the threads on the deque *)
    (* precondition: the list of threads is ordered from oldest to youngest *)
      define @push-threads-on-local-deque-from-atomic (self : vproc, 
						       deque : D.deque, 
						       thds : List.list) : () =
	  fun lp (thds : List.list) : () =
	      case thds
	       of nil => 
		  return ()
		| List.CONS (thd : ImplicitThread.thread, thds : List.list) =>
		  do D.@push-new-end-from-atomic (self, deque, thd)
		  apply lp (thds)
	      end
	  apply lp (thds)
	;

    (* pop all the elements from the deque *)
      define @clear-deque-from-atomic (self : vproc, deque : D.deque) : () =
	  fun lp () : () =
	      let thd : Option.option = D.@pop-new-end-from-atomic (self, deque)
	      case thd
	       of Option.NONE =>
		  return ()
		| Option.SOME (thd : ImplicitThread.thread) =>
		  apply lp ()
	      end
	  apply lp ()
	;

      define @designated-worker (workGroupId : UID.uid,
				 isTerminated : fun (/ -> bool),
				 assignedDeques : Arr.array
			       / exh : exh) : PT.fiber =
	  cont impossible () = 
	   do assert (false)
	   let exn : exn = Fail(@"WorkStealing.@designated-worker: impossible") 
	   throw exh (exn)

	  let nVProcs : int = VProc.@num-vprocs ()

	  cont init (_ : unit) =
	    let self : vproc = SchedulerAction.@atomic-begin ()

	    cont schedulerLoop (deque : D.deque, sign : PT.signal) =

	      cont sigHandler (sign : PT.signal) =
                do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)

		cont dispatch (thd : ImplicitThread.thread) = 
		  do assert (NotEqual (thd, enum(0)))
                  do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
		  do ImplicitThread.@run-from-atomic (self, sigHandler, thd / exh)
		  throw impossible()

		cont findWork () =                
		(* look for work on the local deque. *)
		  let thd : Option.option = D.@pop-new-end-from-atomic (self, deque)
		  do case thd
		      of Option.NONE => 
			 return ()
		       | Option.SOME (thd : ImplicitThread.thread) => 
			 throw dispatch (thd)
		     end
		(* nothing available on the local deque. *)
		  do SchedulerAction.@yield-in-atomic (self)
		(* try to steal from another worker. *)
		  let victimVPId : int = Rand.@in-range-int (0, nVProcs / exh)
		  let victimVP : vproc = VProc.@vproc-by-id (victimVPId)
		  do if Equal (victimVP, self) then 
			 (* cannot steal from the host vproc *)
			 throw findWork()
		     else 
			 return()
		  let stolenThds : List.list = @steal-from-atomic (self, victimVP, workGroupId)
		  do @push-threads-on-local-deque-from-atomic (self, deque, stolenThds)
		  throw findWork ()

		let isFinished : bool = apply isTerminated ()
		do if isFinished then
		       do @clear-deque-from-atomic (self, deque)
		       do D.@release-from-atomic (self, deque)
		       do SchedulerAction.@stop-from-atomic (self)
		       throw impossible ()
		   else
		       return ()

		case sign
		 of PT.STOP =>
		    throw findWork ()
		  | PT.PREEMPT (k : PT.fiber) =>
		    let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		    (*do D.@push-new-end-from-atomic (self, deque, thd) *)
		    do SchedulerAction.@yield-in-atomic (self)
 		    let isFull : bool = D.@is-full (deque)
                    if isFull then
			(* if the deque is full, double its size *)
			let newDeque : D.deque = 
				  D.@double-size-from-atomic (self, workGroupId, deque)
(* TODO *)
			throw impossible ()
		    else
			throw dispatch (thd)
		  | _ =>
		    throw impossible ()
		end (* sigHandler *)

	      throw sigHandler (sign)

	    let deque : D.deque = D.@new-from-atomic (self, workGroupId, INITIAL_DEQUE_SIZE)
            throw schedulerLoop (deque, PT.STOP)

	  return (init)
	;

      )

    in

    _primcode (

    (* return true, if there was an element on the deque *)
      define (* inline *) @pop-new-end (/ exh : exh) : bool =
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

      define (* inline *) @push-new-end (thd : ImplicitThread.thread / exh : exh) : () =
	  let self : vproc = SchedulerAction.@atomic-begin ()
	  let deque : D.deque = @get-assigned-deque-from-atomic (self / exh)
	  let isFull : bool = D.@is-full (deque)
          let deque : D.deque = 
		    (* if the current deque is full, let the scheduler loop choose the course
		     * of action. *)
		      if isFull then
			  do SchedulerAction.@yield-in-atomic (self)
			  let deque : D.deque = @get-assigned-deque-from-atomic (self / exh)
		          return (deque)
		      else
			  return (deque)
	  do D.@push-new-end-from-atomic (self, deque, thd)
	  do SchedulerAction.@atomic-end (self)
	  return ()
	;

      define @work-group (_ : unit / exh : exh) : ImplicitThread.work_group =
	  let uid : UID.uid = UID.@new (/ exh)
	  let terminated : ![bool] = ImplicitThread.@terminated-flag ()
	  fun isTerminated () : bool = return (#0(terminated))
          let nVProcs : int = VProc.@num-vprocs ()
          let assignedDeques : Arr.array = Arr.@array (nVProcs, enum(0):any / exh)
	  let designatedWorkerInit : PT.fiber = @designated-worker (uid, isTerminated, assignedDeques / exh)
	  cont auxiliaryWorkerInit (_ : unit) = 
		let e : exn = Fail(@"WorkStealing.@work-group: todo: implement auxiliary worker")
		throw exh (e)
	  fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
	      do @push-new-end (thd / exh)
	      return (UNIT)
	  fun removeFn (thd : ImplicitThread.thread / exh : exh) : bool = 
	      @pop-new-end (/ exh)
	  let group : ImplicitThread.work_group = 
		      ImplicitThread.@new-work-group (uid,
						      designatedWorkerInit,
						      auxiliaryWorkerInit,
						      spawnFn,
						      removeFn,
						      assignedDeques,
						      terminated
						    / exh)
	  return (group)
	;

    )

    val workGroup : unit -> ImplicitThread.work_group = _prim (@work-group)

    end

  end
