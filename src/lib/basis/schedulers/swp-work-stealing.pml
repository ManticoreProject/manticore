(* swp-work-stealing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Multiprogrammed work stealing algorithm adapted to use software polling.
 *)

#define DEQUE_SZ      1024
#define DEQUE_HD      0
#define DEQUE_TL      1
#define DEQUE_ELTS    2

structure SwpWorkStealing :
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

  end = struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure O = Option

    _primcode(

      extern void* GetNthVProc (int);
      extern void* M_WSAllocLocalDeque (int);

    (* NOTE: the byte layout must match WSLocalDeques_t. *)
      typedef deque = ![
		 long,               (* pointer to the head element. *)
		 long,               (* pointer to the tail element. *)
		 any                 (* array of deque elements. *)
	      ];

    (* array of local deques. *)
      typedef scheduler_data = Arr.array;

    (* number of threads in the local deque (assuming that the head was not stolen) *)
      define @deque-sz (deque : deque / exh : exh) : long =
	  return(I64Sub(SELECT(DEQUE_TL, deque), SELECT(DEQUE_HD, deque)))
	;

    (* update a location in the local deque *)
      define inline @deque-update (deque : deque, i : long, elt : PT.fiber / exh : exh) : () =
	  let elts : addr(any) = &DEQUE_ELTS(deque)
	  let x : bool = ArrayStoreI64(elts, i, elt)
	  return()
	;

    (* subscript from the local deque *)
      define inline @deque-sub (deque : deque, i : long / exh : exh) : PT.fiber =
	  let elts : addr(any) = &DEQUE_ELTS(deque)
	  let elt : any = ArrayLoadI64(elts, i)
	  return((PT.fiber)elt)
	;

      define inline @is-deque-empty (deque : deque / exh : exh) : bool =
	  let sz : long = @deque-sz(deque / exh)
	  return(I64Lt(sz, 1))
	;

    (* pop from the tail of the deque *)
      define inline @pop-tl-from-atomic (self : vproc, deque : deque / exh : exh) : PT.fiber =
	  let tl : long = I64Sub(SELECT(DEQUE_TL, deque), 1)
	  let x : any = @deque-sub(deque, tl / exh)
	  do UPDATE(DEQUE_TL, deque, tl)
	  do @deque-update(deque, tl, NIL_FIBER / exh)
	  do assert(NotEqual(x, nil))
	  return(x)
	;

    (* push on the tail of the deque *)
      define inline @push-tl-from-atomic (self : vproc, deque : deque, elt : PT.fiber / exh : exh) : () =
	  do assert(NotEqual(deque, nil))
	  do assert(NotEqual(elt, nil))
	  let tl : long = SELECT(DEQUE_TL, deque)
	  do @deque-update(deque, tl, elt / exh)
	  do UPDATE(DEQUE_TL, deque, I64Add(tl, 1))
	  return()
	;

    (* pop from the head of the deque *)
      define inline @pop-hd-from-atomic (self : vproc, deque : deque / exh : exh) : PT.fiber =
	  let hd : long = SELECT(DEQUE_HD, deque)
	  let x : any = @deque-sub(deque, hd / exh)
	  do UPDATE(DEQUE_HD, deque, I64Add(hd, 1))
	  do @deque-update(deque, hd, NIL_FIBER / exh)
	  do assert(NotEqual(x, nil))
	  return(x)
	;

    (* allocate the local deque *)
      define @alloc-deque (self : vproc, deques : Arr.array) : deque =
	  let id : int = VProc.@vproc-id(self)
	  let deque : deque = ccall M_WSAllocLocalDeque(id)
	  do ArrayStoreI64(deques, id, deque)
	  return(deque)
        ;

    (* try to steal from a victim vproc.
     * PRECONDITION: NotEqual(self, victim)
     *)
      define @try-to-steal-from-atomic (self : vproc, victim : vproc, victimDeque : deque / exh) : O.option =
	  let resp : ![O.option] = alloc(O.NONE)
	  let resp : ![O.option] = promote(resp)
        (* access the victim's local deque from the victim's vproc. *)
	  cont thief (_ : unit) =
	    let thd : O.option = @pop-hd-from-atomic(self, victimDeque / exh)
	    let thd : O.option = promote(thd)
	    do #0(resp) := thd
	    SchedulerAction.@stop()
        (* busy wait for the theif to respond. *)
	  fun waitForResp () : O.option =
	      case #0(resp)
	       of O.NONE =>
		  do Pause()
		  do SchedulerAction.@yield-in-atomic(self)
		  apply waitForResp()
		| O.SOME (thd : ImplicitThread.thread) =>
		  return(#0(resp))
	      end
	  do VProc.@send-high-priority-signal-from-atomic(self, victim, thief)
	  apply waitForResp()
	;

      define @worker (workerID : int,                (* worker id *)
		      nWorkers : int,                (* total number of workers in the group *)
                      deques : Arr.array,            (* contains pointers to every deque in the work group *)
                      barrier : NWayBarrier.barrier
		     / exh : exh) : PT.fiber =

	  cont initK (_ : unit) =
	      let self : vproc = SchedulerAction.@atomic-begin()
	      let deque : deque = @alloc-deque(self, deques / exh)
	      do NWayBarrier.@ready(barrier / exh)
	    (* wait for the other schedulers to allocate their local deques *)
	      do NWayBarrier.@barrier(barrier / exh)

	      cont schedulerLoop (sign : PT.signal) =
		let self : vproc = host_vproc

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
			then throw findWork()
		     else return()
		  let thd : O.option = @try-to-steal-from-atomic(self, victimVP, ArrayLoadI64(deques, victim) / exh)
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
		    do @push-tl-from-atomic(self, deque, thd / exh)
		    do SchedulerAction.@yield-in-atomic(self)
		    throw findWork()
		  | _ =>
		    throw impossible()
		end
	    throw schedulerLoop(PT.STOP)

	  return(initK)
	;

    (* get a pointer to the deque that is local to the host vproc *)
      define @get-deque-from-atomic (self : vproc / exh : exh) : deque =
	  let schedulerData : any = ImplicitThread.@get-scheduler-data(/ exh)
	  let schedulerData : scheduler_data = (scheduler_data)schedulerData
	  let id : int = VProc.@vproc-id(self)
	  let deque : deque = ArrayLoadI64(#0(schedulerData), id)
	  return(deque)
	;

    (* pop from the tail of the local deque *)
      define @pop-tl ( / exh : exh) : bool =
	  let vp : vproc = SchedulerAction.@atomic-begin()
	  let deque : deque = @get-deque-from-atomic(vp / exh)
	  let kOpt : O.option = @pop-tl-from-atomic(deque / exh)
	  do SchedulerAction.@atomic-end(vp)
	  let isNonEmpty : bool = 
		case kOpt
		 of O.NONE => return (false)
		  | O.SOME(k : ImplicitThread.thread) => return(true)
		 end
	  return(isNonEmpty)
	;

    (* push on the tail of the local deque *)
      define @push-tl (thd : ImplicitThread.thread / exh : exh) : () =
	  let vp : vproc = SchedulerAction.@atomic-begin()
	  let deque : deque = @get-deque-from-atomic(vp / exh)
	  do @push-tl-from-atomic(deque, thd / exh)
	  do SchedulerAction.@atomic-end(vp)
	  return()
	;

    (* initialize the scheduler on all vprocs *)
      define @work-group (x : unit / exh : exh) : ImplicitThread.group =
	  let nVPs : int = VProc.@num-vprocs()
	  let deques : Arr.array = Arr.@array(nVPs, enum(0) / exh)
	  fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
	      let vp : vproc = SchedulerAction.@atomic-begin()
	      let id : int = VProc.@vproc-id(vp)
	      do @push-tl-from-atomic(ArrayLoadI64(deques, id), thd / exh)
	      do SchedulerAction.@atomic-end(vp)
	      return(UNIT)
	  fun removeFn (thd : ImplicitThread.thread / exh : exh) : bool = 
	      let vp : vproc = SchedulerAction.@atomic-begin()
	      let id : int = VProc.@vproc-id(vp)
	      let x : Option.option = @pop-tl-from-atomic(ArrayLoadI64(deques, id) / exh)
	      do SchedulerAction.@atomic-end(vp)
	      case x
	       of Option.NONE => 
		  return(true)
		| Option.SOME(x : ImplicitThread.thread) => 
		  return(false)
	      end
	  cont init (x : unit) =
	    let workerId : int = VProc.@vproc-id(host_vproc)
	    let init : PT.fiber = @worker(workerId, nVPs, deques / exh)
	    throw init(UNIT)
	  let group : ImplicitThread.group = ImplicitThread.@group(init, spawnFn, removeFn, deques / exh)
	  return(group)
	;

    )

    val workGroup : unit -> ImplicitThread.group = _prim(@work-group)

  end
