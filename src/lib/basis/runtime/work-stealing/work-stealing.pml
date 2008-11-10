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

#define WORKER_LOCAL_DEQUE_OFF              0
#define WORKER_LOCAL_DEQUE_GLOBAL_LIST_OFF  1

#define NIL_FIBER                   (PT.fiber)enum(0):any

structure WorkStealing =
  struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure FLS = FiberLocalStorage
    structure O = Option

    _primcode(

      extern void* GetNthVProc (int);
      extern void* M_WSAllocLocalDeque (int);
      extern void* M_WSGetLocalDeque (void*);
      extern void M_WSFreeLocalDeque (void*);

      typedef deque_elts = any;
      typedef local_deque = ![
                       long,               (* hd - points at the head fiber *)
		       long,               (* tl - points one above the tail fiber *)
		       deque_elts          (* array of fibers *)
                    ];
    (* worker in the work-stealing pool *)
      typedef worker = ![
                local_deque,               (* local deque *)
		any                        (* pointer into the global list of local deques (used for
					    * explicitly de-allocating the local deque) *)
              ];

    (* find the local worker in the array of workers *)
      define @find-local-worker (workers : Arr.array / exh : exh) : worker =
	let id : int = VProc.@id(host_vproc / exh)
	let worker : any = Arr.@sub(workers, id / exh)
	return((worker)worker)
      ;

    (* get the pointer to the current worker *)
      define @get-local-worker (/ exh : exh) : worker =
        let workers : any = ThreadCapabilities.@get-from-fls(tag(workStealingWorker) / exh)
        @find-local-worker((Arr.array)workers / exh)
      ;

    (* get the pointer to the local deque *)
      define @get-local-deque (/ exh : exh) : local_deque =
	let worker : worker = @get-local-worker(/ exh)
	return(SELECT(WORKER_LOCAL_DEQUE_OFF, worker))
      ;

    (* allocate the local deque on the host vproc *)
      define @alloc-local-deque (worker : worker / exh : exh) : local_deque =
	let id : int = VProc.@id(host_vproc / exh)
	let localDequeGlobalList : any = ccall M_WSAllocLocalDeque(id)
	let localDeque : local_deque = ccall M_WSGetLocalDeque(localDequeGlobalList)
	do UPDATE(WORKER_LOCAL_DEQUE_OFF, worker, localDeque)
	do UPDATE(WORKER_LOCAL_DEQUE_GLOBAL_LIST_OFF, worker, localDequeGlobalList)
	return(localDeque)
      ;

    (* number of threads in the local deque (assuming that the head was not stolen) *)
      define @local-deque-sz (localDeque : local_deque / exh : exh) : long =
        return(I64Sub(SELECT(LOCAL_DEQUE_HD, localDeque), SELECT(LOCAL_DEQUE_TL, localDeque)))
      ;

    (* update a location in the local deque *)
      define @deque-update (localDeque : local_deque, i : long, elt : PT.fiber / exh : exh) : () =
	let elts : addr(any) = &LOCAL_DEQUE_ELTS(localDeque)
	let x : bool = ArrayStoreI64(elts, i, elt)
	return()
      ;

    (* subscript from the local deque *)
      define @deque-sub (localDeque : local_deque, i : long / exh : exh) : PT.fiber =
	let elts : addr(any) = &LOCAL_DEQUE_ELTS(localDeque)
        let elt : any = ArrayLoadI64(elts, i)
	return((PT.fiber)elt)
      ;

      define @is-local-deque-empty (localDeque : local_deque / exh : exh) : bool =
	let sz : long = @local-deque-sz(localDeque / exh)
	return(I64Lt(sz, 1))
      ;

    (* pop from the tail of the local deque *)
      define @local-deque-pop-tl (localDeque : local_deque / exh : exh) : PT.fiber =
        let tl : long = I64Sub(SELECT(LOCAL_DEQUE_TL, localDeque), 1)
        let x : any = @deque-sub(localDeque, tl / exh)
        do UPDATE(LOCAL_DEQUE_TL, localDeque, tl)
        do @deque-update(localDeque, tl, NIL_FIBER / exh)
        do assert(NotEqual(x, nil))
        return(x)
      ;

    (* push on the tail of the local deque *)
      define @local-deque-push-tl (localDeque : local_deque, elt : PT.fiber / exh : exh) : () =
        do assert(NotEqual(localDeque, nil))
	do assert(NotEqual(elt, nil))
        let tl : long = SELECT(LOCAL_DEQUE_TL, localDeque)
(*
do ccall M_PrintPtr("localDeque", localDeque)
do ccall M_PrintPtr("tl", &LOCAL_DEQUE_TL(localDeque))
do ccall M_PrintLong(SELECT(LOCAL_DEQUE_TL, localDeque))
do print_ppt()
*)
        do @deque-update(localDeque, tl, elt / exh)
        do UPDATE(LOCAL_DEQUE_TL, localDeque, I64Add(tl, 1))
        return()
      ;

    (* pop from the head of the local deque *)
      define @local-deque-pop-hd (localDeque : local_deque / exh : exh) : PT.fiber =
        let hd : long = SELECT(LOCAL_DEQUE_HD, localDeque)
        let x : any = @deque-sub(localDeque, hd / exh)
        do UPDATE(LOCAL_DEQUE_HD, localDeque, I64Add(hd, 1))
        do @deque-update(localDeque, hd, NIL_FIBER / exh)
        do assert(NotEqual(x, nil))
        return(x)
      ;

    (* send a messenger that will attempt to steal from a victim worker. *)
      define @request-steal (victimVP : vproc / exh : exh) : O.option =
	let ch : VProcChan.chan = VProcChan.@new(/ exh)
      (* the thief attempts to deque from the victim's local deque. *)
	fun thief (x : unit / exh : exh) : O.option =
	    let localDeque : local_deque = @get-local-deque(/ exh)
	    let sz : long = @local-deque-sz(localDeque / exh)
	    if I64Eq(sz, 0)
	       then return(O.NONE)
	    else
		let stolenK : PT.fiber = @local-deque-pop-hd(localDeque / exh)
		return(O.SOME(stolenK))
      (* we run the thief on the victim vproc *)
	do VProcChan.@messenger(ch, victimVP, thief / exh)
	let stolenK : any = VProcChan.@recv-spin(ch / exh)
	return((O.option)stolenK)
      ;

    (* attempt to steal a thread  *)
      define @steal (worker : worker, workers : Arr.array / exh : exh) : O.option =
        let nWorkers : int = Arr.@length(workers / exh)
	let victim : int = Rand.@in-range-int(0, nWorkers / exh)
        let victimVP : vproc = ccall GetNthVProc(victim) 
        if NotEqual(host_vproc, victimVP)
	   then @request-steal(victimVP / exh)
	else return(O.NONE)
      ;

    (* create an instance of the scheduler for a vproc *)
      define @scheduler (workers : Arr.array, self : vproc / exh : exh) : PT.sched_act =
        let worker : worker = @find-local-worker(workers / exh)
	let localDeque : local_deque = @alloc-local-deque(worker / exh)        
      (* scheduler loop *)
	cont switch (sign : PT.signal) =
        (* run a thread *)
	  cont run (switch : PT.sched_act, k : PT.fiber) =
	    do Control.@run(switch, k / exh)
            let e : exn = Fail(@"impossible")
	    throw exh(e)
        (* steal a thread from a remote vproc *)
	  cont steal (switch : PT.sched_act) =
	    let kOpt : O.option = @steal(worker, workers / exh)
	    case kOpt
	     of O.NONE => throw steal(switch)
	      | O.SOME (k : PT.fiber) => throw run(switch, k)
	    end
        (* handle a signal *)
	  case sign
	   of PT.STOP =>
	      let isEmpty : bool = @is-local-deque-empty(localDeque / exh)
              if isEmpty
		 then throw steal(switch)
	      else
		  let k : PT.fiber = @local-deque-pop-tl(localDeque / exh)
                  throw run(switch, k)
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
      define @init (/ exh : exh) : Arr.array =
      (* one worker per vproc *)
	let nVProcs : int = VProc.@num-vprocs(/ exh)
        let workers : Arr.array = Arr.@array(nVProcs, $0 / exh)
	fun f (i : int / exh : exh) : () =
            if I32Lt(i, nVProcs)
	       then
                let worker : worker = alloc(M_NIL, M_NIL)
                do Arr.@update(workers, i, worker / exh)
                apply f(I32Add(i, 1) / exh)
	    else return()
      (* allocate the worker structures *)
        do apply f(0 / exh)
      (* initialize the scheduler *)
	let vps : List.list = VProc.@all(/ exh)
	let fls : FLS.fls = FLS.@get( / exh)
	fun mkAct (self : vproc / exh : exh) : PT.sched_act = @scheduler(workers, self / exh)
	do SchedulerUtils.@scheduler-startup(mkAct, fls, vps / exh)  
	return(workers)
      ;

    (* pop from the tail *)
      define @pop-tl(/ exh : exh) : bool =
        let localDeque : local_deque = @get-local-deque(/ exh)
        let isEmpty : bool = @is-local-deque-empty(localDeque / exh)
        if isEmpty
	   then return(false)
	else
	   let k : PT.fiber = @local-deque-pop-tl(localDeque / exh)
           return(true)
      ;

    (* push on the tail *)
      define @push-tl(k : PT.fiber / exh : exh) : () =
        let localDeque : local_deque = @get-local-deque(/ exh)	
        @local-deque-push-tl(localDeque, k / exh)
      ;

    )

  end
