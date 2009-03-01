(* multiprogrammed-work-stealing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Multiprogrammed work-stealing scheduler described by Arora et. al. (SPAA 1998).
 *)

structure MultiprogrammedWorkStealing :
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


      typedef scheduler_data = [
                     Arr.array        (* deques *)
              ];

      define @worker (workerID : int,                (* worker id *)
		      nWorkers : int,                (* total number of workers in the group *)
                      deques : Arr.array		      
		     / exh : exh) : PT.fiber =

	let deque : Cilk5Deque.deque = Arr.@sub(deques, workerID / exh)

	cont schedulerLoop (sign : PT.signal) =
          let self : vproc = host_vproc

          cont impossible () = 
            do assert(false)
            let exn : exn = Fail(@"MultiprogrammedWorkStealing: impossible")
            throw exh(exn)

	  cont dispatch (thd : ImplicitThread.thread) = 
            do assert(NotEqual(thd, enum(0)))
	    do ImplicitThread.@run-in-scheduler(schedulerLoop, thd / exh)
	    throw impossible()

        (* attempt to steal from another worker *)
	  cont steal () =
            do SchedulerAction.@yield-in-atomic(self)
	    let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	    do if I32Eq(victim, workerID)
		  then throw steal()
	       else return()
	    let victimDeque : Cilk5Deque.deque = Arr.@sub(deques, victim / exh)
	    let thd : O.option = Cilk5Deque.@pop-hd-from-atomic(victimDeque / exh)
	    case thd
	     of O.NONE =>
		throw steal()
	      | O.SOME(thd : ImplicitThread.thread) =>
		 throw dispatch (thd)
	    end
        
	  case sign
	   of PT.STOP =>
	      let thd : O.option = Cilk5Deque.@pop-tl-from-atomic(deque / exh)
	      case thd
	       of O.NONE => throw steal()
		| O.SOME(thd : ImplicitThread.thread) => throw dispatch(thd)
	      end
	    | PT.PREEMPT (k : PT.fiber) =>
	      let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
	      do Cilk5Deque.@push-tl-from-atomic(deque, thd / exh)
              do SchedulerAction.@yield-in-atomic(self)
	      throw schedulerLoop(PT.STOP)
	    | _ =>
	      throw impossible()
	  end

        cont initK (x : unit) = throw schedulerLoop(PT.STOP)
	return(initK)
      ;

    (* get a pointer to the deque that is local to the host vproc *)
      define @get-local-deque (/ exh : exh) : Cilk5Deque.deque =
	let schedulerData : any = ImplicitThread.@get-scheduler-data(/ exh)
	let schedulerData : scheduler_data = (scheduler_data)schedulerData
	let workerId : int = VProc.@vproc-id(host_vproc)
	let deque : Cilk5Deque.deque = Arr.@sub(SELECT(0, schedulerData), workerId / exh)
	return(deque)
      ;

    (* pop from the tail of the local deque *)
      define @pop-tl ( / exh : exh) : bool =
	let deque : Cilk5Deque.deque = @get-local-deque( / exh)
        let vp : vproc = SchedulerAction.@atomic-begin()
	let kOpt : O.option = Cilk5Deque.@pop-tl-from-atomic(deque / exh)
        do SchedulerAction.@atomic-end(vp)
	let isNonEmpty : bool = 
	      case kOpt
	       of O.NONE => return (false)
		| O.SOME(k : ImplicitThread.thread) => return(true)
	       end
	return(isNonEmpty)
      ;

    (* push on the tail of the local deque *)
      define @push-tl(thd : ImplicitThread.thread / exh : exh) : () =
	let deque : Cilk5Deque.deque = @get-local-deque( / exh)
        let vp : vproc = SchedulerAction.@atomic-begin()
	do Cilk5Deque.@push-tl-from-atomic(deque, thd / exh)
        do SchedulerAction.@atomic-end(vp)
        return()
      ;

    (* initialize the scheduler on all vprocs *)
      define @work-group (x : unit / exh : exh) : ImplicitThread.group =
        let nVPs : int = VProc.@num-vprocs()
	let deques : Arr.array = Arr.@array(nVPs, enum(0) / exh)
	fun initDeque (vp : vproc / exh : exh) : () =
	    let workerID : int = VProc.@vproc-id(vp)
	    let deque : Cilk5Deque.deque = Cilk5Deque.@new(/ exh)
	    Arr.@update(deques, workerID, deque / exh)
	do VProc.@for-each-vproc(initDeque / exh)

        let schedulerData : scheduler_data = alloc(deques)
	    
	fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
            let workerId : int = VProc.@vproc-id(host_vproc)
            let deque : Cilk5Deque.deque = Arr.@sub(deques, workerId / exh)
            let vp : vproc = SchedulerAction.@atomic-begin()
	    do Cilk5Deque.@push-tl-from-atomic(deque, thd / exh)
            do SchedulerAction.@atomic-end(vp)
	    return(UNIT)

        fun removeFn (thd : ImplicitThread.thread / exh : exh) : bool = 
	    let workerId : int = VProc.@vproc-id(host_vproc)
            let deque : Cilk5Deque.deque = Arr.@sub(deques, workerId / exh)
            let vp : vproc = SchedulerAction.@atomic-begin()
            let x : Option.option = Cilk5Deque.@pop-tl-from-atomic(deque / exh)
            do SchedulerAction.@atomic-end(vp)
            case x
	     of Option.NONE => return(true)
	      | Option.SOME(x : ImplicitThread.thread) => return(false)
            end

        cont init (x : unit) =
          let workerId : int = VProc.@vproc-id(host_vproc)
          let init : PT.fiber = @worker(workerId, nVPs, deques / exh)
          throw init(UNIT)

	let group : ImplicitThread.group = ImplicitThread.@group(init, spawnFn, removeFn, schedulerData / exh)

	return(group)
      ;

    )

    val workGroup : unit -> ImplicitThread.group = _prim(@work-group)

  end
