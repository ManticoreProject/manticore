(* work-stealing.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The Work Stealing scheduler.
 *)

#define DEFAULT_DEQUE_SZ            64

#define STEAL_ONE_THREAD            0
#define STEAL_TWO_THREADS           1
#define STEAL_HALF_DEQUE            2

#define STEAL_POLICY                STEAL_ONE_THREAD

structure WorkStealing (* :
  sig

    val workGroup : unit -> ImplicitThread.work_group

    val isLocalDequeEmpty : unit -> bool

  end *) = struct

    local

      structure Arr = Array64
      structure D = WorkStealingDeque
      structure PT = PrimTypes

      _primcode (
        
      (* returns the assigned deque for the host vproc *)
	define inline @get-assigned-deque-from-atomic (self : vproc / exh : exh) : D.deque =
	    let deques : any = ImplicitThread.@get-scheduler-state (/ exh)
            do assert (NotEqual (deques, enum(0):any))
	    let id : int = VProc.@vproc-id (self)
	    let deque : D.deque = Arr.@sub ((Arr.array)deques, id / exh)
	    do assert (NotEqual (deque, enum(0):any))
 	    return (deque)
	  ;
        
      (* set the assigned deque for the host vproc *)
	define inline @set-assigned-deque-from-atomic (self : vproc, 
							     assignedDeques : Arr.array,
							     deque : D.deque / exh : exh) : () =
	    let id : int = VProc.@vproc-id (self)
	    do Arr.@update (assignedDeques, id, deque / exh)
	    return ()
	  ;

(*
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

*)

      (* steal several threads. the list of returned threads is in order from oldest to youngest. the maximum
       * number of threads stolen is determined by the MAX_NUM_TO_STEAL parameter.
       *
       * PRECONDITION: this function is applied while executing on the victim vproc with signals masked
       *)
	define @steal-threads-from-atomic (
                                  victimVP : vproc,                         (* victim vproc *)
				  workGroupId : UID.uid,                    (* work group *)
				  policy : int                              (* policy to determine the number of threads to steal, which
									     * can be one of the following:
									     *     STEAL_ONE_THREAD
									     *     STEAL_TWO_THREADS
									     *     STEAL_HALF_DEQUE
									     *) 
                              ) : (* ImplicitThread.thread *) List.list =

          (** find the candidate deques **)
	    fun dequeSzGtOne (deque : [D.deque] / exh : exh) : bool =
		let size : int = D.@size (#0(deque))
                if I32Gt(size, 1) then return (true) else return (false)
	    let localDeques : List.list = D.@local-deques-from-atomic (victimVP, workGroupId)
            cont exh (_ : exn) = return (List.nil)
          (* deques containing at least one element *)
	    let candidateDeques : List.list = PrimList.@filter (dequeSzGtOne, localDeques / exh)

	    let stolenThds : List.list = 
		      case candidateDeques
		       of List.nil =>
			  return (List.nil)
			| List.CONS (deque : [D.deque], _ : List.list) =>
			  let deque : D.deque = #0(deque)
			  fun take (nLeft : int, stolen : List.list) : List.list =
			      if I32Lte (nLeft, 0) then
				  PrimList.@rev (stolen / exh)
			      else
				  let thdOpt : Option.option = D.@pop-old-end-from-atomic (victimVP, deque)
				  case thdOpt
				   of NONE =>
				      PrimList.@rev (stolen / exh)
				    | SOME (thd : ImplicitThread.thread) =>
				      apply take (I32Sub (nLeft, 1), List.CONS (thd, stolen))
				  end
			  if I32Eq (policy, STEAL_ONE_THREAD) then
			      apply take (1, List.nil)
			  else if I32Eq (policy, STEAL_TWO_THREADS) then
			      apply take (2, List.nil)
			  else if I32Eq (policy, STEAL_HALF_DEQUE) then
			      let n : int = D.@size (deque)
			      apply take (I32Div (n, 2), List.nil)
                          else
			      throw exh (Match)
		      end

	    do D.@release-deques-from-atomic (victimVP, localDeques)
	    return (stolenThds)
	  ;

      (* send a thief from thiefVP to steal from victimVP. the result list is the list of stolen threads. *)
	define @thief-from-atomic (thiefVP : vproc, victimVP : vproc, workGroupId : UID.uid, wid : long, policy : int)
								      : (* ImplicitThread.thread *) List.list =
	    do assert (NotEqual (thiefVP, victimVP))
            let tid : long = Logging.@log-WSThiefSend (thiefVP, wid)
	  (* communication channel used to pass the result of the steal from the victim vproc back to the
	   * thief. the channel is initially nil, but once the steal attempt completes the channel is
	   * seeded with the list of stolen threads.
	   *)
	    let ch : ![(* ImplicitThread.thread List.list *) Option.option] = alloc (Option.NONE)
	    let ch : ![(* ImplicitThread.thread List.list *) Option.option] = promote (ch)
	  (* the thief fiber executes on the victim vproc *)
	    cont thief (_ : unit) =
	      let self : vproc = SchedulerAction.@atomic-begin ()
              do Logging.@log-WSThiefBegin (self, tid, wid)
	      do assert (Equal (self, victimVP))
              let stolenThds : List.list = @steal-threads-from-atomic (self, workGroupId, policy)
              do case stolenThds
		  of List.nil =>
		     (* failed steal attempt *)
		     let x : Option.option = promote (Option.SOME(List.nil))
		     do #0(ch) := x
		     return ()
		   | List.CONS (_ : ImplicitThread.thread, _ : List.list) => 
		     (* successfully stole multiple threads *)
		     let x : Option.option = promote (Option.SOME(stolenThds))
		     do #0(ch) := x
		     return ()
		 end
              do Logging.@log-WSThiefEnd (self, tid, wid)
	      SchedulerAction.@stop ()
	  (* send the thief fiber to the victim vproc *)
	    let fls : FLS.fls = FLS.@get()
	    do VProc.@send-from-atomic (thiefVP, victimVP, fls, thief)
	    fun wait () : List.list =
		case #0(ch)
		 of Option.NONE =>
		    do Pause()
		    let _ : vproc = SchedulerAction.@yield-in-atomic (thiefVP)
		    apply wait ()
		  | Option.SOME (thds : List.list) =>
		    do case thds
			of List.nil => Logging.@log-WSThiefUnsuccessful (thiefVP, tid, wid)
			 | _ => Logging.@log-WSThiefSuccessful (thiefVP, tid, wid)
                       end
		    return (thds)
		end
	  (* wait for the thief to report its findings *)
	    apply wait ()
	  ;

	define @new-worker (workGroupId : UID.uid, 
			    wgid : long,                   (* unique id for logging *)
			    isTerminated : fun (/ -> bool), 
			    setActive : fun (bool / ->),
  		            assignedDeques : Arr.array,
		            idle : (* bool *) Arr.array,
                            pickVictimVProc : fun (vproc / -> (* [vproc] *) Option.option)
			  / exh : exh)                           : cont (vproc, ImplicitThread.worker) =
	    cont impossible () = 
	      do assert_fail()
	      throw exh (Fail(@"WorkStealing.@designated-worker: impossible"))

	    cont initWorker (self : vproc, worker : ImplicitThread.worker) =	      
	      let deque : D.deque = D.@new-from-atomic (self, workGroupId, DEFAULT_DEQUE_SZ)
	      do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
              let wid : long = Logging.@log-WSWorkerInit (self, wgid)

	      cont schedulerLoop (self : vproc, deque : D.deque, sign : PT.signal) =
		let vpId : int = VProc.@vproc-id (self)
		let workerFLS : FLS.fls = FLS.@get ()
		cont dispatch (thd : ImplicitThread.thread) = 
	          do Logging.@log-WSExecute (self, wid)
		  cont act (sign : PT.signal) = throw schedulerLoop (self, deque, sign)
		  do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
		  do ImplicitThread.@run-from-atomic (self, act, thd / exh)
		  throw impossible ()
		case sign
		 of PT.STOP =>
		  (* restart the worker loop *)
		    cont foundWork (self : vproc, thds : (* ImplicitThread.thread *) List.list) =
		      do D.@add-list-from-atomic (self, deque, thds)
		      do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
		      do Arr.@update (idle, vpId, false / exh)
		      throw schedulerLoop (self, deque, PT.STOP)
		  (* try to find work for the given vproc. the result is a list of stolen work. *)
		    fun findRemoteWork (self : vproc) : List.list =
			let victimVP : Option.option = apply pickVictimVProc (self)
			case victimVP
			 of Option.NONE =>
			    (* no victims currently available *)
			    return (List.nil)
			  | Option.SOME (victimVP : [vproc]) =>
			    let victimVP : vproc = #0(victimVP)
			    let victimId : int = VProc.@vproc-id (victimVP)
			    let idle : bool = Arr.@sub (idle, victimId / exh)
			    case idle
			     of true =>
				return (List.nil)
			      | false =>
				@thief-from-atomic (self, victimVP, workGroupId, wid, STEAL_POLICY)
			    end
			end
		   let waitFn : fun (/ -> bool) = SpinWait.@mk-spin-wait-fun (20)
		 (* loop until work appears on the local deque, or a steal attempt succeeds *)
		   cont findRemoteWorkLp (nTries : int) =
		     let self : vproc = SchedulerAction.@atomic-begin ()
		   (* it is important to yield here, since we want to give other threads a chance
		    * to add work to the local deque.
		    *)
		     let self : vproc = SchedulerAction.@yield-in-atomic (self)
		     let thd : Option.option = D.@pop-new-end-from-atomic (self, deque)
		     do case thd
			 of Option.NONE =>
			    (* there was no local work available on the other workers *)
			     return ()
			   | Option.SOME (thd : ImplicitThread.thread) =>
			     throw foundWork (self, List.CONS(thd, List.nil))
			 end
		     let stolenThds : List.list = apply findRemoteWork (self)
		     case stolenThds
		      of List.nil =>
		       (* our steal attempt failed *)
			 let nVProcs : int = VProc.@num-vprocs ()
			 if I32Gt (nTries, nVProcs) then
			   (* this worker exceeded its maximum number of consecutive steal attempts. now we
			    * spin wait for a while so that we avoid flooding busy workers by making
			    * too many steal attempts.
			    *)

			     do SchedulerAction.@atomic-end (self)
                             do apply setActive (false)
			     let reset : bool = apply waitFn ()
			     do case reset
				 of true =>
				    do Logging.@log-WSSleep (self, wid)
				    do SchedulerAction.@sleep (1000000:long)
				    return ()
				  | false =>
				    return()
				end
		             let terminated : bool = apply isTerminated ()
                             do case terminated
				 of true => 
				    do Logging.@log-WSTerminate (self, wgid)
				    let _ : unit = SchedulerAction.@stop() return ()
				  | false => return ()
                                end
                             do apply setActive (true)              
			     throw findRemoteWorkLp (0)
			 else
			     throw findRemoteWorkLp (I32Add(nTries, 1))
		       | List.CONS (thd : ImplicitThread.thread, thds : List.list) =>
		       (* successful steal *)
			 throw foundWork (self, stolenThds)
		     end
		   let thd : Option.option = D.@pop-new-end-from-atomic (self, deque)
		   case thd
		    of Option.NONE =>
		       (* there is no local work *)
		       do Arr.@update (idle, vpId, true / exh)
		       throw findRemoteWorkLp (0)
		     | Option.SOME (thd : ImplicitThread.thread) =>
		       throw dispatch (thd)
		   end
		 | PT.PREEMPT (k : PT.fiber) =>
		   let isFull : bool = D.@is-full (deque)
		   case isFull
		    of true => (* the deque is full: resize the deque and then preempt the thread *)
		       let newDeque : D.deque = D.@double-size-from-atomic (self, workGroupId, deque)
		       throw schedulerLoop (self, newDeque, sign)
		     | false =>
		       let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		     (* make the thread available to other workers *)
		       do D.@push-new-end-from-atomic (self, deque, thd)
		       do Logging.@log-WSPreempted (self, wid)
		       let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		       throw schedulerLoop (self, deque, PT.STOP)
		  end
		| _ =>
		   throw impossible ()
	       end (* schedulerLoop *)

	      throw schedulerLoop (self, deque, PT.STOP)
	    return (initWorker)
	  ;

      (* return true, if there was an element on the deque *)
	define inline @remove-thread (/ exh : exh) : bool =
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

	define inline @spawn-thread (thd : ImplicitThread.thread / exh : exh) : () =
	    fun lp (self : vproc) : () =
		let deque : D.deque = @get-assigned-deque-from-atomic (self / exh)
		let isFull : bool = D.@is-full (deque)
		case isFull
		 of true =>  (* the deque is full: let the scheduler loop choose the next action *)
		    PRINT_DEBUG("WorkStealing: full deque")
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
	define inline @resume-thread-on-new-deque (thd : ImplicitThread.thread / exh : exh) : () =
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
	    let nVProcs : int = VProc.@num-vprocs ()
          (* when #0(nIdle) = nVProcs the work, the work group is finished *)
            let nIdle : ![int] = alloc (0)
            let nIdle : ![int] = promote (nIdle)
          (* true, if the work group is finished *)
            let terminated : ![bool] = alloc (false)
            let terminated : ![bool] = promote (terminated)
          (* mark a worker as active by applying this function to "true" and inactive by applying to "false" *)
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
	    fun pickVictimVProc (self : vproc) : (* [vproc] *) Option.option =
		if I32Lte (nVProcs, 1) then
		    return (Option.NONE)
		else
		    let victimVPId : int = Rand.@in-range-int (0, nVProcs / exh)
		    let victimVP : vproc = VProc.@vproc-by-id (victimVPId)
		    if Equal (victimVP, self) then 
			(* never steal from the host vproc *)
			apply pickVictimVProc (self)
		    else
			return (Option.SOME (alloc (victimVP)))
	    let nVProcs : int = VProc.@num-vprocs ()
	    let assignedDeques : Arr.array = Arr.@array (nVProcs, enum(0):any / exh)
            let idle : Arr.array = Arr.@array (nVProcs, false / exh)
            let self : vproc = SchedulerAction.@atomic-begin ()
            let wgid : long = Logging.@log-WSInit (self)
            do SchedulerAction.@atomic-end (self)
	    let initWorker : cont (vproc, ImplicitThread.worker) = 
			     @new-worker (uid, wgid, isTerminated, setActive, assignedDeques, idle, pickVictimVProc / exh)
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
		let worker : Word64.word = ImplicitThread.@spawn-worker (dst, fls, initWorker / exh)
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

    end

  end
