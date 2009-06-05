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

  end *) = struct

    local

      _primcode (

#define INITIAL_DEQUE_SIZE   128

      (* returns the assigned deque for the host vproc *)
	define inline @get-assigned-deque-from-atomic (self : vproc / exh : exh) : WorkStealingDeque.deque =
	    let deques : any = ImplicitThread.@get-scheduler-data (/ exh)
	    let id : int = VProc.@vproc-id (self)
	    let deque : WorkStealingDeque.deque = Arr.@sub ((Arr.array)deques, id / exh)
	    do assert (NotEqual (deque, nil))
	    return (deque)
	  ;

      (* set the assigned deque for the host vproc *)
	define inline @set-assigned-deque-from-atomic (self : vproc, 
						       deque : WorkStealingDeque.deque / exh : exh) : () =
	    let deques : any = ImplicitThread.@get-scheduler-data (/ exh)
	    let id : int = VProc.@vproc-id (self)
	    do Arr.@update ((Arr.array)deques, id, deque / exh)
	    return (deque)
	  ;

      (* returns threads stolen from the victim vproc *)
      (* precondition: NotEqual (self, victim) *)
	define @steal-from-atomic (self : vproc, victim : vproc, workGroupId : long) : List.list =
	    do assert (NotEqual (self, victim))

	    let res : ![Option.option] = alloc (Option.NONE)
	    let res : ![Option.option] = promote(res)

	    cont thief (_ : unit) =
	      let self : vproc = SchedulerAction.@atomic-begin()
	      let deques : List.list = WorkStealingDeque.@local-deques-from-atomic (self, workGroupId)
	      fun selectNonEmptyDeque (deques : List.list) : Option.option =
		  case deques
		   of nil => 
		      return (Option.NONE)
		    | List.CONS (deque : WorkStealingDeque.deque, deques : List.list) =>
		      let isEmpty : bool = WorkStealingDeque.@is-empty-from-atomic (self, deque)
		      if isEmpty then
			  apply selectNonEmptyDeque (deques)
		      else
			  return (Option.SOME(deque))
		  end
	      let deque : Option.option = apply selectNonEmptyDeque (deques)
	      case deque
	       of Option.NONE =>
		(* found no available deques *)
		  do #0(res) := List.nil
		  do SchedulerAction.@atomic-end(self)
		  SchedulerAction.@stop()
		| Option.SOME (deque : WorkStealingDeque.deque) =>
		  let thd : Option.option = @pop-new-end-from-atomic (self, deque)
		  case thd
		   of Option.NONE => 
		    (* the deque returned by selectNonEmptyDeque should not be empty *)
		      do assert (false)
		      SchedulerAction.@stop()
		    | Option.SOME (thd : ImplicitThread.thread) =>
		      let x : List.list = List.CONS (thd, nil)
		      let x : List.list = promote (x)
		      do #0(res) := x
		      do SchedulerAction.@atomic-end(self)
		      SchedulerAction.@stop()
		  end
	      end (* thief *)

	    fun wait () : List.list =
		case #0(resp)
		 of Option.NONE =>
		    do Pause()
		    do SchedulerAction.@yield-in-atomic (self)
		    apply waitForResp()
		  | Option.SOME (thds : List.list) =>
		    return (thds)
		end

	  (* wait for the thief to report its findings *)
	    apply wait ()
	  ;

    (* push the threads on the deque *)
    (* precondition: the list of threads is ordered from oldest to youngest *)
      define @push-threads-on-local-deque-from-atomic (self : vproc, 
						       deque : WorkStealingDeque.deque, 
						       thds : List.list) : () =
	  fun lp (thds : List.list) : () =
	      case thds
	       of nil => 
		  return ()
		| List.CONS (thd : ImplicitThread.thread, thds : List.list) =>
		  do WorkStealingDeque.@push-new-end-from-atomic (self, deque, thd)
		  apply lp (thds)
	  apply lp ()
	;

    (* pop all the elements from the deque *)
      define @clear-deque-from-atomic (self : vproc, deque : deque) : () =
	  fun lp () : () =
	      let thd : Option.option = WorkStealingDeque.@pop-new-end-from-atomic (self, deque)
	      case thd
	       of Option.NONE =>
		  return ()
		| Option.SOME (thd : ImplicitThread.thread) =>
		  apply lp ()
	      end
	  apply lp ()
	;

      define @designated-worker-from-atomic (self : vproc, 
					     workGroup : ImplicitThread.work_group,
					     isWorkGroupFinished : fun ( -> bool)
					    / exh : exh) : PT.fiber =
	  cont impossible () = 
	   do assert (false)
	   let exn : exn = Fail(@"WorkStealing: impossible") 
	   throw exh (exn)

	  let workGroupId : long = ImplicitThread.@work-group-id (workGroup)
	  let nVProcs : int = VProc.@num-vprocs ()

	  cont init (_ : unit) =
	    let self : vproc = SchedulerAction.@atomic-begin()
	    let deque : WorkStealingDeque.deque = 
			  WorkStealingDeque.@new-from-atomic (self, workGroupId, INITIAL_DEQUE_SIZE)

	    cont schedulerLoop (sign : PT.signal) =

	      cont dispatch (thd : ImplicitThread.thread) = 
		do assert (NotEqual (thd, enum(0)))
		do ImplicitThread.@run-in-scheduler (self, schedulerLoop, thd / exh)
		throw impossible()

	      cont findWork () =
                let isFinished : bool = apply isWorkGroupFinished ()
                do if isFinished then
		       do @clear-deque-from-atomic (self, deque)
		       do WorkStealingDeque.@release-from-atomic (self, deque)
                       do SchedulerAction.@stop-from-atomic (self)
                       throw impossible ()
		   else
		       return ()
	      (* look for work on the local deque. *)
		let thd : O.option = WorkStealingDeque.@pop-new-end-from-atomic (self, deque)
		do case thd
		    of O.NONE => 
		       return ()
		     | O.SOME (thd : ImplicitThread.thread) => 
		       throw dispatch (thd)
		   end
	      (* nothing available on the local deque. *)
		do SchedulerAction.@yield-in-atomic (self)
	      (* try to steal from another worker. *)
		let victimVPId : int = Rand.@in-range-int(0, nVProcs / exh)
		let victimVP : vproc = VProc.@vproc-by-id(victimVPId)
		do if Equal(victimVP, self) then 
		       (* cannot steal from the host vproc *)
		       throw findWork()
		   else 
		       return()
		let stolenThds : List.list = @steal-from-atomic (self, victimVP, workGroupId)
		do apply @push-threads-on-local-deque-from-atomic (self, deque, stolenThds)
		throw findWork ()

	      case sign
	       of PT.STOP =>
		  throw findWork ()
		| PT.PREEMPT (k : PT.fiber) =>
		  let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		  do @push-new-end-from-atomic (self, deque, thd / exh)
		  do SchedulerAction.@yield-in-atomic(self)
		  throw findWork()
		| _ =>
		  throw impossible()
	      end (* schedulerLoop *)

	    throw schedulerLoop(PT.STOP)

	  return (init)
	;

      )

    in

    _primcode (

    (* return true, if there was an element on the deque *)
      define inline @pop-new-end (/ exh) : bool =
	  let self : vproc = SchedulerAction.@atomic-begin ()
	  let deque : WorkStealingDeque.deque = @get-assigned-deque-from-atomic (self / exh)
	  let k : Option.option = WorkStealingDeque.@pop-new-end-from-atomic (self, deque)
	  do SchedulerAction.@atomic-end (self)
	  case k
	   of Option.NONE =>
	      return (false)
	    | Option.some (k : ImplicitThread.thread) =>
	      return (true)
	  end
	;

      define inline @push-new-end (thd : ImplicitThread.thread / exh : exh) : () =
	  let self : vproc = SchedulerAction.@atomic-begin ()
	  let deque : WorkStealingDeque.deque = @get-assigned-deque-from-atomic (self / exh)
	  let isFull : bool = WorkStealingDeque.@is-full (deque)
	  let deque : WorkStealingDeque.deque =
		      if isFull then
			(* if the deque is full, double its size and continue *)
                          let workGroup : ImplicitThread.work_group = ImplicitThread.@peek (UNIT / exh)
			  let workGroupId : long = ImplicitThread.@work-group-id (workGroup)
			  let newDeque : WorkStealingDeque.deque = 
					   WorkStealingDeque.@double-size-from-atomic (self, workGroupId, deque)
			  do @set-assigned-deque-from-atomic (self, newDeque)
			  return (newDeque)
		      else
			  return (deque)
	  do WorkStealingDeque.@push-new-end-from-atomic (self, deque, thd)
	  do SchedulerAction.@atomic-end (self)
	  return ()
	;

    )

    end

  end
