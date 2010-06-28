(* work-stealing.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The Work Stealing scheduler.
 *)

#define DEFAULT_DEQUE_SZ            128

structure WorkStealing (* :
  sig

    val workGroup : unit -> ImplicitThread.work_group

  end *) = struct

    local

      structure Arr = Array64
      structure D = Cilk5Deque
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

	define @mug-from-atomic (self : vproc, workGroupId : UID.uid) : (* ImplicitThread.thread *) List.list =
(*
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
					| CONS (deque : [D.deque], _ : List.list) => 
					  D.@to-list-from-atomic (self, #0(deque))
	                              end
            do D.@release-deques-from-atomic (self, localDeques)
            return (threads)
*)
return (List.nil)
	  ;

	define @new-worker (workGroupId : UID.uid, 
			    wgid : long,                   (* unique id for logging *)
			    isTerminated : fun (/ -> bool), 
  		            assignedDeques : Arr.array,
		            idle : (* bool *) Arr.array,
                            pickVictim : fun (vproc / -> (* [vproc] *) Option.option)
			  / exh : exh)                           : cont (vproc, ImplicitThread.worker) =
	    cont impossible () = 
	      do assert_fail()
	      throw exh (Fail(@"WorkStealing.@designated-worker: impossible"))

	    cont initWorker (self : vproc, worker : ImplicitThread.worker) =
              let wid : long = Logging.@log-WSWorkerInit (self, wgid)
              let vpId : int = VProc.@vproc-id (self)              
              let deque : D.deque = Arr.@sub (assignedDeques, vpId / exh)

	      cont schedulerLoop (self : vproc, deque : D.deque, sign : PT.signal) =
		let workerFLS : FLS.fls = FLS.@get ()
		cont dispatch (thd : ImplicitThread.thread) = 
	          do Logging.@log-WSExecute (self, wid)
		  cont act (sign : PT.signal) = throw schedulerLoop (self, deque, sign)
		  do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh)
		  do ImplicitThread.@run-from-atomic (self, act, thd / exh)
		  throw impossible ()
		let isFinished : bool = apply isTerminated ()
		do case isFinished
		    of true =>
		       do SchedulerAction.@stop-from-atomic (self)
		       throw impossible ()
		     | false =>
		       return ()
		   end
		case sign
		 of PT.STOP =>
		  (* restart the worker loop *)
		    cont foundWork (self : vproc, thds : (* ImplicitThread.thread *) List.list) =
		      do D.@add-list-from-atomic (deque, thds / exh)
		      do @set-assigned-deque-from-atomic (self, assignedDeques, deque / exh) 
		      do Arr.@update (idle, vpId, false / exh)
		      throw schedulerLoop (self, deque, PT.STOP)
		  (* try to find work for the given vproc. the result is a list of stolen work. *)
		    fun findRemoteWork (self : vproc) : List.list =
		       (* try to mug another deque that is local to this vproc *)
			 let muggedThreads : List.list = @mug-from-atomic (self, workGroupId)
			 case muggedThreads
			  of CONS (_ : ImplicitThread.thread, _ : List.list) =>
			    (* mugging was successful *)
			    return (muggedThreads)
			  | List.nil =>
			    (* mugging failed; try to steal *)
			    let victimVP : Option.option = apply pickVictim (self)
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
                                    let victimDeque : D.deque = Arr.@sub (assignedDeques, victimId / exh)
				    let res : Option.option = D.@pop-old-end-from-atomic (victimDeque / exh)            
				    case res
				     of Option.NONE => 
					return (List.nil)
				      | Option.SOME (thd : ImplicitThread.thread) =>
					return (CONS(thd, List.nil))
				    end
				end
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
		     let thd : Option.option = D.@pop-new-end-from-atomic (deque / exh)
		     do case thd
			 of Option.NONE =>
			    (* there was no local work available on the other workers *)
			     return ()
			   | Option.SOME (thd : ImplicitThread.thread) =>
			     throw foundWork (self, CONS(thd, List.nil))
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
			     let reset : bool = apply waitFn ()
			     do case reset
				 of true =>
(*				    do Logging.@log-WSSleep (self, wid)
				    do SchedulerAction.@sleep (700000:long)*)
				    return ()
				  | false =>
				    return()
				end                            
			     throw findRemoteWorkLp (0)
			 else
			     throw findRemoteWorkLp (I32Add(nTries, 1))
		       | CONS (thd : ImplicitThread.thread, thds : List.list) =>
		       (* successful steal *)
			 throw foundWork (self, stolenThds)
		     end
		   let thd : Option.option = D.@pop-new-end-from-atomic (deque / exh)
		   case thd
		    of Option.NONE =>
		       (* there is no local work *)
		       do Arr.@update (idle, vpId, true / exh)
		       throw findRemoteWorkLp (0)
		     | Option.SOME (thd : ImplicitThread.thread) =>
		       throw dispatch (thd)
		   end
		 | PT.PREEMPT (k : PT.fiber) =>
		   let thd : ImplicitThread.thread = ImplicitThread.@capture (k / exh)
		 (* make the thread available to other workers *)
		   do D.@push-new-end-from-atomic (deque, thd / exh)
		   do Logging.@log-WSPreempted (self, wid)
		   let _ : vproc = SchedulerAction.@yield-in-atomic (self)
		   throw schedulerLoop (self, deque, PT.STOP)
(*
		   let _ : vproc = SchedulerAction.@yield-in-atomic (self)
throw dispatch (thd)
*)
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
	    let k : Option.option = D.@pop-new-end-from-atomic (deque / exh)
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
		do D.@push-new-end-from-atomic (deque, thd / exh)
		do SchedulerAction.@atomic-end (self)
		return ()
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    apply lp (self)
	  ;

      (* one of several policies for resuming a thread. here we create a deque containing just the given thread. *)
	define inline @resume-thread-on-new-deque (thd : ImplicitThread.thread / exh : exh) : () =
do assert (I32Eq(1,0))
return()
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
            do assert (NotEqual (assignedDeques, enum(0):any))
	    fun f (vp : vproc / exh : exh) : () =
		let deque : D.deque = D.@new (/ exh)
	        do @set-assigned-deque-from-atomic (vp, assignedDeques, deque / exh)
                return ()
            do VProc.@for-each-vproc (f / exh)
            let idle : Arr.array = Arr.@array (nVProcs, false / exh)
            let self : vproc = SchedulerAction.@atomic-begin ()
            let wgid : long = Logging.@log-WSInit (self)
            do SchedulerAction.@atomic-end (self)
	    let initWorker : cont (vproc, ImplicitThread.worker) = 
			     @new-worker (uid, wgid, isTerminated, assignedDeques, idle, pickVictim / exh)
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

      )

    in

    val workGroup : unit -> ImplicitThread.work_group = _prim (@work-group)

    end

  end
