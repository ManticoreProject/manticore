(* swp-work-stealing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Multiprogrammed work stealing algorithm adapted to use software polling.
 *)

structure SwpWorkStealing (* :
  sig

  (*
    _prim(

    (* pop from the tail of the local deque *)
      define @pop-tl ( / exh : exh) : bool;

    (* push on the tail of the local deque *)
      define @push-tl (thd : ImplicitThread.thread / exh : exh) : ();
    )

  *)

    val workGroup : unit -> ImplicitThread.group

  end *) = struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure O = Option

#define DEQUE_SZ      1024
#define DEQUE_HD      0
#define DEQUE_TL      1
#define DEQUE_ELTS    2

#define NIL           enum(0):any

    _primcode(

      extern void* GetNthVProc (int);
      extern void* M_WSAllocLocalDeque (int);

    (* NOTE: the byte layout must match WSLocalDeques_t. *)
      typedef deque = ![
		 long,               (* pointer to the head element. *)
		 long,               (* pointer to the tail element. *)
		 any                 (* array of deque elements. *)
	      ];

    )

    local

    _primcode (

    (* number of threads in the deque. *)
      define @deque-sz (deque : deque / exh : exh) : long =
	  return(I64Sub(SELECT(DEQUE_TL, deque), SELECT(DEQUE_HD, deque)))
	;

      define inline @is-deque-empty (deque : deque / exh : exh) : bool =
	  let sz : long = @deque-sz(deque / exh)
	  return(I64Lt(sz, 1))
	;

    (* update a location in the local deque. *)
      define inline @deque-update (deque : deque, i : long, elt : any / exh : exh) : () =
	  let elts : addr(any) = &DEQUE_ELTS(deque)
	  do ArrayStore((any)elts, i, elt)
	  return()
	;

    (* subscript from the deque. *)
      define inline @deque-sub (deque : deque, i : long / exh : exh) : any =
	  let elts : addr(any) = &DEQUE_ELTS(deque)
	  let elt : any = ArrayLoad((any)elts, i)
	  return(elt)
	;

    (* pop from the tail of the deque. *)
      define inline @pop-tl-from-atomic (self : vproc, deque : deque / exh : exh) : O.option =
	  do assert(NotEqual(deque, NIL))
          let isEmpty : bool = @is-deque-empty(deque / exh)
          if isEmpty
	     then
	      return(O.NONE)
	  else
	      let tl : long = I64Sub(SELECT(DEQUE_TL, deque), 1)
	      let x : any = @deque-sub(deque, tl / exh)
	      do UPDATE(DEQUE_TL, deque, tl)
	      do @deque-update(deque, tl, NIL / exh)
	      do assert(NotEqual(x, NIL))
	      return(O.SOME(x))
	;

    (* push on the tail of the deque. *)
      define inline @push-tl-from-atomic (self : vproc, deque : deque, elt : any / exh : exh) : () =
	  do assert(NotEqual(deque, NIL))
	  do assert(NotEqual(elt, NIL))
	  let tl : long = SELECT(DEQUE_TL, deque)
	  do @deque-update(deque, tl, elt / exh)
	  do UPDATE(DEQUE_TL, deque, I64Add(tl, 1))
	  return()
	;

    (* pop from the head of the deque. *)
      define inline @pop-hd-from-atomic (self : vproc, deque : deque / exh : exh) : O.option =
	  do assert(NotEqual(deque, NIL))
          let isEmpty : bool = @is-deque-empty(deque / exh)
          if isEmpty
	     then
	      return(O.NONE)
	  else
	      let hd : long = SELECT(DEQUE_HD, deque)
	      let x : any = @deque-sub(deque, hd / exh)
	      do UPDATE(DEQUE_HD, deque, I64Add(hd, 1))
	      do @deque-update(deque, hd, NIL / exh)
	      do assert(NotEqual(x, NIL))
	      return(O.SOME(x))
	;

    (* allocate the deque for a single worker. *)
      define @alloc-deque (self : vproc, deques : Arr.array / exh : exh) : deque =
	  let id : int = VProc.@vproc-id(self)
	  let deque : deque = ccall M_WSAllocLocalDeque(id)
          do Arr.@update(deques, id, deque / exh)
	  return(deque)
        ;

    (* attempt to steal a thread from the victim. our protocol sends a fiber to the victim's vproc. the
     * thief waits for a response, which either states that no work is available or contains the stolen
     * thread.
     * PRECONDITION: NotEqual(self, victim)
     *)
      define @try-to-steal-from-atomic (self : vproc, victim : vproc, victimDeque : deque / exh : exh) : O.option =
	  let resp : ![O.option] = alloc(O.NONE)
	  let resp : ![O.option] = promote(resp)
        (* the thief's fiber, which runs on the victim vproc *)
	  cont thief (_ : unit) =
            let self : vproc = SchedulerAction.@atomic-begin()
	    let thd : O.option = @pop-hd-from-atomic(self, victimDeque / exh)
	    let thd : O.option = promote(O.SOME(thd))
	    do #0(resp) := thd
            do SchedulerAction.@atomic-end(self)
	    SchedulerAction.@stop()
        (* busy wait until the thief responds. *)
	  fun waitForResp () : O.option =
	      case #0(resp)
	       of O.NONE =>
		  do Pause()
		  do SchedulerAction.@yield-in-atomic(self)
		  apply waitForResp()
		| O.SOME (thd : Option.option) =>
		  return(thd)
	      end
	  do VProc.@send-high-priority-signal-from-atomic(self, victim, thief)
	  apply waitForResp()
	;

    (* get a pointer to the deque that is local to the host vproc *)
      define inline @get-deque-from-atomic (self : vproc / exh : exh) : deque =
	  let deques : any = ImplicitThread.@get-scheduler-data(/ exh)
	  let id : int = VProc.@vproc-id(self)
	  let deque : deque = Arr.@sub((Arr.array)deques, id / exh)
	  return(deque)
	;

      define @worker-from-atomic (nWorkers : int,                (* total number of workers in the group *)
                                  deques : Arr.array,            (* contains pointers to every deque in the work group *)
                                  barrier : Barrier.barrier
		                 / exh : exh) : PT.fiber =
	  cont initK (_ : unit) =
	      let self : vproc = SchedulerAction.@atomic-begin()
	      let workerId : int = VProc.@vproc-id(self)
	      let deque : deque = @alloc-deque(self, deques / exh)
	      do Barrier.@ready(barrier / exh)
	    (* wait for the other schedulers to allocate their local deques *)
	      do Barrier.@wait(barrier / exh)

	      cont schedulerLoop (sign : PT.signal) =
		cont impossible () = 
		  do assert(false)
		  let exn : exn = Fail(@"SwpWorkStealing: impossible")
		  throw exh(exn)

		cont dispatch (thd : ImplicitThread.thread) = 
		  do assert(NotEqual(thd, enum(0)))
		  do ImplicitThread.@run-in-scheduler(self, schedulerLoop, thd / exh)
		  throw impossible()

		cont findWork () =
		(* look for work on the local deque. *)
		  let thd : O.option = @pop-tl-from-atomic(self, deque / exh)
		  do case thd
		      of O.NONE => 
			 return()
		       | O.SOME(thd : ImplicitThread.thread) => 
			 throw dispatch(thd)
		     end
		(* nothing available on the local deque. *)
		  do SchedulerAction.@yield-in-atomic(self)
		(* try to steal from another worker. *)
		  let victim : int = Rand.@in-range-int(0, nWorkers / exh)
                  let victimVP : vproc = VProc.@vproc-by-id(victim)
		  do if Equal(victimVP, self)
			then 
			 throw findWork()
		     else 
			 return()
		  let victimDeque : deque = Arr.@sub(deques, victim / exh)
		  let thd : O.option = @try-to-steal-from-atomic(self, victimVP, victimDeque / exh)
		  case thd
		   of O.NONE =>
		      throw findWork()
		    | O.SOME(thd : ImplicitThread.thread) =>
		      throw dispatch(thd)
		  end

		case sign
		 of PT.STOP =>
		    throw findWork ()
		  | PT.PREEMPT (k : PT.fiber) =>
		    let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
(* FIXME: it should be possible to put the thread back on the queue before yielding. *)
		    (*do @push-tl-from-atomic(self, deque, thd / exh)*)
		    do SchedulerAction.@yield-in-atomic(self)
		    (*throw findWork()*)
                    throw dispatch(thd)
		  | _ =>
		    throw impossible()
		end
	      (* end schedulerLoop *)
	    throw schedulerLoop(PT.STOP)
	  return(initK)      (* end initK *)
	;

    )

    in

    _primcode (


    (* pop from the tail of the local deque *)
      define inline @pop-tl ( / exh : exh) : bool =
	  let vp : vproc = SchedulerAction.@atomic-begin()
	  let deque : deque = @get-deque-from-atomic(vp / exh)
	  let kOpt : O.option = @pop-tl-from-atomic(vp, deque / exh)
	  do SchedulerAction.@atomic-end(vp)
	  let isNonEmpty : bool = 
		case kOpt
		 of O.NONE => return (false)
		  | O.SOME(k : ImplicitThread.thread) => return(true)
		 end
	  return(isNonEmpty)
	;

    (* push on the tail of the local deque *)
      define inline @push-tl (thd : ImplicitThread.thread / exh : exh) : () =
	  let vp : vproc = SchedulerAction.@atomic-begin()
	  let deque : deque = @get-deque-from-atomic(vp / exh)
	  do @push-tl-from-atomic(vp, deque, thd / exh)
	  do SchedulerAction.@atomic-end(vp)
	  return()
	;

    (* initialize the scheduler on all vprocs *)
      define @work-group (x : unit / exh : exh) : ImplicitThread.group =
	  let nVPs : int = VProc.@num-vprocs()
	  let deques : Arr.array = Arr.@array(nVPs, enum(0) / exh)
          let barrier : Barrier.barrier = Barrier.@new(nVPs / exh)
	  fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
	      do @push-tl (thd / exh)
	      return(UNIT)
	  fun removeFn (thd : ImplicitThread.thread / exh : exh) : bool = 
	      @pop-tl (/ exh)
	  let initWorker : PT.fiber = @worker-from-atomic(nVPs, deques, barrier / exh)
	  let group : ImplicitThread.group = ImplicitThread.@group(initWorker, spawnFn, removeFn, deques / exh)
        (* wait for the deques to all be initialized. *)
          do Barrier.@wait(barrier / exh)
	  return(group)
	;

    )

    val workGroup : unit -> ImplicitThread.group = _prim(@work-group)
						   
    end (* local *)

  end
