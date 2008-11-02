(* work-stealing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of the Cilk-5 work stealing algorithm that has been adapted to 
 * Manticore.
 *
 * The C runtime support is in parallel-rt/misc/work-stealing-local-deques.c.
 *)

#define LOCAL_DEQUE_SZ      1024
#define LOCAL_DEQUE_HD      0
#define LOCAL_DEQUE_TL      1
#define LOCAL_DEQUE_ELTS    2

#define WORKER_LOCAL_DEQUE_OFF  0
#define WORKER_THIEF_INBOX_OFF  1

#define INBOX_EMPTY                $0
#define INBOX_STEAL_FAILED         $1

structure WorkStealing =
  struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure FLS = FiberLocalStorage
    structure O = Option

    _primcode(

      extern void* GetNthVProc (int);
      extern void M_WSAllocLocalDeques(void*, int);

      typedef deque_elts = any;
      typedef thief_inbox = ![PT.fiber];   (* shared location for passing stolen fibers *)
      typedef local_deque = ![
                       int,                (* hd - points at the head fiber *)
		       int,                (* tl - points one above the tail fiber *)
		       deque_elts          (* array of fibers *)
                    ];
    (* each worker has a local deque and an inbox for stolen fibers *)
      typedef worker = [
                local_deque,               (* local deque *)
		thief_inbox                (* thief inbox *)
              ]

    (* get the pointer to the current worker *)
      define @get-worker (/ exh : exh) : worker =
        let worker : any = ThreadCapabilities.@get-from-fls(tag(workStealingWorker) / exh)
	return((worker)worker)
      ;

    (* get the pointer to the local deque *)
      define @get-local-deque (/ exh : exh) : local_deque =
	let worker : worker = @get-worker(/ exh)
	return(SELECT(WORKER_LOCAL_DEQUE_OFF, worker))
      ;

    (* get the thief's inbox *)
      define @get-inbox (/ exh : exh) : thief_inbox =
	let worker : worker = @get-worker(/ exh)
	return(SELECT(WORKER_THIEF_INBOX_OFF, worker))
      ;

    (* number of threads in the local deque (assuming that the head was not stolen) *)
      define @local-deque-sz (localDeque : local_deque / exh : exh) : int =
        return(I32Sub(SELECT(LOCAL_DEQUE_HD, localDeque), SELECT(LOCAL_DEQUE_TL, localDeque)))
      ;

    (* pop from the tail of the local deque *)
      define @local-deque-pop-tl (localDeque : local_deque / exh : exh) : PT.fiber =
        let elts : deque_elts = SELECT(LOCAL_DEQUE_ELTS, localDeque)
        let tl : int = I32Sub(SELECT(LOCAL_DEQUE_TL, localDeque), 1)
        let x : any = ArrayLoadI64(elts, tl)
        do UPDATE(LOCAL_DEQUE_TL, localDeque, tl)
        let b : bool = ArrayStoreI64(elts, tl, nil)
        do assert(NotEqual(x, nil))
        return(x)
      ;

    (* pop from the head of the local deque *)
      define @local-deque-pop-hd (localDeque : local_deque / exh : exh) : PT.fiber =
	let elts : deque_elts = SELECT(LOCAL_DEQUE_ELTS, localDeque)
        let hd : int = SELECT(LOCAL_DEQUE_HD, localDeque)
        let x : any = ArrayLoadI64(elts, hd)
        do UPDATE(LOCAL_DEQUE_HD, localDeque, I32Add(hd, 1))
        let b : bool = ArrayStoreI64(elts, hd, nil)
        do assert(NotEqual(x, nil))
        return(x)
      ;

    (* push on the tail of the local deque *)
      define @local-deque-push-tl (localDeque : local_deque, elt : PT.fiber / exh : exh) : () =
        let tl : int = SELECT(LOCAL_DEQUE_TL, localDeque)
        do UPDATE(LOCAL_DEQUE_TL, localDeque, I32Add(tl, 1))
        let x : bool = ArrayStoreI64(SELECT(LOCAL_DEQUE_ELTS, localDeque), tl, elt)
        return()
      ;

    (* put the stolen fiber in the thief's inbox *)
      define @place-in-inbox (inbox : thief_inbox, stolenK : O.option / exh : exh) : () =
	do case stolenK
	    of O.NONE => 
	       do UPDATE(0, inbox, INBOX_STEAL_FAILED)
	     | O.SOME(k : PT.fiber) =>
	       let k : PT.fiber = promote(k)
	       do UPDATE(0, inbox, k)
	    end
	return()
      ;

    (* send a messenger that will attempt to steal from a victim worker. we place the stolen fiber
     * in the thief's inbox. 
     *)
      define @request-steal (vp : vproc, inbox : thief_inbox / exh : exh) : O.option =
        cont thiefK (x : PT.unit) =
          let localDeque : local_deque = @get-local-deque(/ exh)
          let sz : int = @local-deque-sz(localDeque / exh)
          let stokenK : O.option = 
            if I32Eq(sz, 0)
	       then return(O.NONE)
	    else
	      let stokenK : PT.fiber = @local-deque-pop-hd(localDeque / exh)
	      return(O.SOME(stolenK))
          do @place-in-inbox(inbox, stolenK / exh)
	  let _ : PT.unit = Control.@stop(/ exh)
	  return()
      (* busy wait for a response from the thief *)
        fun wait () : O.option =
	    if Equal(#0(inbox), INBOX_EMPTY)
	       then apply wait()
	    else if Equal(#0(inbox), INBOX_STEAL_FAILED)
	       then return(O.NONE)
	    else return(O.SOME(#0(inbox)))
        let k : O.option = apply wait ()
      (* prepare the inbox for the next steal attempt *)
        do UPDATE(0, inbox, INBOX_EMPTY)
        return(k)
      ;

    (* attempt to steal a thread  *)
      define @steal (workers : Arr.array / exh : exh) : O.option =
	cont exit () = return(O.NONE)
	let id : int = SchedulerUtils.@vproc-id(host_vproc / exh)
        let nWorkers : int = Arr.@length(workers / exh)
	let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	do if I32Eq(victim, id)
	      then throw exit()
	   else return()
        let victimVP : vproc = ccall GetNthVProc(victim) 
        let worker : worker = Arr.sub(workers, victim / exh)
        @request-steal(victimVP, SELECT(WORKER_THIEF_INBOX_OFF,worker) / exh)
      ;

    (* create an instance of the scheduler for a vproc *)
      define @scheduler (workers : Arr.array, self : vproc / exh : exh) : PT.sched_act =
	let nWorkers : int = Arr.@length(workers / exh)
	let id : int = SchedulerUtils.@vproc-id(self / exh)
	let localDeque : local_deque = @get-local-deque(/ exh)
      (* scheduler loop *)
	cont switch (sign : PT.signal) =
        (* run a thread *)
	  cont run (switch : PT.sched_act, k : PT.fiber) =
	    do Control.@run(switch, k / exh)
            let e : exn = Fail(@"impossible")
	    throw exh()
        (* steal a thread from a remote vproc *)
	  cont steal (switch : PT.sched_act) =
	    let kOpt : O.option = @steal(workers / exh)
	    case kOpt
	     of O.NONE => throw steal(switch)
	      | O.SOME (k : PT.fiber) => throw run(switch, k)
	    end
        (* handle a signal *)
	  case sign
	   of PT.STOP =>
	      let elt : O.option = @local-deque-pop-tl(localDeque / exh)
	      case elt
	       of O.NONE => throw steal(switch)
		| O.SOME(k : PT.fiber) => throw run(switch, k)
	      end
	    | PT.PREEMPT (k : PT.fiber) =>
	      let _ : PT.unit = Control.@atomic-yield(/exh)
              throw run(switch, k)
	    | _ =>
	      let e : exn = Match
     	      throw exh(e)
          end

	return(switch)
      ;

    (* initialize the workers *)
      define @init (/ exh : exh) : PT.unit =
      (* one worker per vproc *)
	let nVProcs : int = SchedulerUtils.@num-vprocs(/ exh)
      (* allocate local deques *)
	let localDeques : any = ccall M_WSAllocLocalDeques(host_vproc, nVProcs)
        let workers : Arr.array = Arr.array(nVProcs, $0 / exh)
	fun f (i : int / exh : exh) : () =
            if I32Lte(i, nVProcs)
	       then
		let localDeque : local_deque = (local_deque)ArrayI64Sub(localDeques, i)
                let thiefInbox : thief_inbox = alloc(INBOX_EMPTY)
                let worker : worker = alloc(localDeque, thiefInbox)
                do Arr.update(workers, i, worker)
                apply f(I32Add(i, 1) / exh)
	    else return()
      (* allocate the worker structures *)
        do apply f(0 / exh)
      (* initialize the scheduler *)
	let vps : List.list = SchedulerUtils.@all-vprocs(/ exh)
	let fls : FLS.fls = FLS.@get( / exh)
	fun mkAct (self : vproc / exh : exh) : PT.sched_act = @scheduler(workers, self / exh)
	do SchedulerUtils.@scheduler-startup(mkAct, fls, vps / exh)  
	return(UNIT)
      ;

    (* pop from the tail *)
      define @pop-tl(/ exh : exh) : bool =
        let localDeque : local_deque = @get-local-deque(/ exh)
	let elt : O.option = @local-deque-pop-tl(localDeque / exh)
	case elt
	 of O.NONE => return(false)
	  | O.SOME(k : PT.fiber) => return(true)
	end
      ;

    (* push on the tail *)
      define @push-tl(k : PT.fiber / exh : exh) : () =
        let localDeque : local_deque = @get-local-deque(/ exh)	
        @local-deque-push-tl(localDeque, k / exh)
      ;

    )

  end
