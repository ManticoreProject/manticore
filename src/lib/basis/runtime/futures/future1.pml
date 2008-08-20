(* future1.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Runtime support for futures where at most one fiber can perform a touch operation.
 *)

(* state values *)
#define EMPTY_F  $0
#define STOLEN_F $1
#define EVAL_F   $2

(* offsets *)
#define STATE_OFF            0
#define THUNK_OFF            1
#define CANCELABLE_OFF       2
#define FGS_OFF              3

structure Future1 : FUTURE = struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage
    structure C = Cancelation
    structure LQ = LockedQueue

    type thunk = unit -> 'a
    _primcode ( typedef thunk = fun (PT.unit / PT.exh -> any); )

    (* the future1 structure contains:
     *     1) a _state_ word, with one of the following values:
     *          EMPTY_F
     *          STOLEN_F
     *          EVAL_F
     *          FULL      value
     *          WAITING   cont
     *     2) a _thunk_ word 
     *     3) a cancelable for canceling the future's evaluation
     *     4) fiber-local storage for the future (tracks parent->child relationships)
     *)

    type 'a future = _prim ( ![any, thunk, C.cancelable, FLS.fls] )

    _primcode (

	define @eval (fut : future / exh : PT.exh) : any =
	  let f : thunk = SELECT(THUNK_OFF, fut)
	 (* clear the thunk pointer to avoid a space leak *)
	  do UPDATE(THUNK_OFF, fut, (thunk) $0)
	  let result : any = apply f (UNIT / exh)
	  return(result)
	;

	define @steal (futuresQ : LQ.queue, fut : future / exh : PT.exh) : () =
	  let tmp : any = CAS (&0(fut), EMPTY_F, STOLEN_F)
	  if Equal (tmp, EMPTY_F) 
	     then let result : any = @eval(fut / exh)
		  let tmpX : any = CAS(&0(fut), STOLEN_F, result)
		  if Equal (tmpX, STOLEN_F)
		     then return ()
		     else (* unblock the future *)
			  do UPDATE(STATE_OFF, fut, result)
			  let k : PT.fiber = (PT.fiber) tmpX
			  do LQ.@enqueue (futuresQ, k / exh)
			  return ()
	      else (* future cell is already full *)
		   return ()
	;

      (* gang scheduler: takes a shared readyQ and the vproc id of the worker, and returns
       * a scheduler action that represents an instance of the scheduler for a vproc.
       *)
        define @gang-sched (readyQ : LockedQueue.queue, self : vproc / exh : PT.exh) : PT.sigact =
          cont switch (s : PT.signal) =
          (* sanity check *)
            do assert(Equal(self, host_vproc))
          (* get the next available future *)
            cont dispatch () =
		let k : Option.option = LockedQueue.@dequeue(readyQ / exh)
                case k
		 of NONE =>
                  (* nothing on the queue; so, we yield *)
		    let _ : PT.unit = Control.@atomic-yield(/exh)
                    throw switch(STOP)
		  | SOME(k : PT.fiber) =>
		  (* got a future; so, we run it now *)
		    do Control.@run(switch, k / exh)
                    throw dispatch()
                end
          (* handle signals *)
            case s
	     of STOP => 
		throw dispatch()
	      | PT.PREEMPT(k : PT.fiber) =>
              (* mugging policy: other workers can steal k *)
              (* QUESTION: does this policy work well for a shared FIFO queue? *)
		do LockedQueue.@enqueue(readyQ, k / exh)
		let _ : PT.unit = Control.@atomic-yield(/exh)
		throw dispatch()
            end
          return(switch)                
        ;

    (* initialize the gang scheduler if necessary (we do this job just once); return the ready queue for
     * the scheduler
     *)
      define @init-gang-sched (c : SetOnceMem.set_once_mem / exh : PT.exh) : LockedQueue.queue =
        let fls : FLS.fls = FLS.@get( / exh)
        fun init (x : PT.unit / exh : PT.exh) : any =
          (* create the ready queue *)
            let readyQ : LockedQueue.queue = LockedQueue.@new(/exh)
          (* initialize the scheduler on all vprocs *)
            let vps : List.list = ccall ListVProcs(host_vproc)
            fun initOnVProc (self : vproc / exh : PT.exh) : PT.sigact =
                  @gang-sched(readyQ, self / exh)
            do SchedulerUtils.@scheduler-startup(initOnVProc, fls, vps / exh)
            let readyQ : any = (any)readyQ
            return(readyQ)
        let readyQ : any = SetOnceMem.@set(c, init / exh)
        let readyQ : LockedQueue.queue = (LockedQueue.queue)readyQ
        return(readyQ)
      ;

    (* get a handle on the ready queue *)
      define @get-ready-queue ( / exh : PT.exh) : LockedQueue.queue =
        let fls : FLS.fls = FLS.@get( / exh)
        let futureSched : Option.option = FLS.@find(fls, tag(future1GangSched) / exh)
        case futureSched
	 of NONE => 
          (* this thread does not support futures *)
	    do assert(FALSE)
            return($0)
	  | Option.SOME (c : SetOnceMem.set_once_mem) =>
          (* initialize the scheduler *)
            let readyQ : LockedQueue.queue = @init-gang-sched(c / exh)
            return(readyQ)
        end                                            
      ;

    (* initial value for the thread capability *)
      define @capability-init (/ exh : PT.exh) : ThreadCapabilities.capability =
	let c : SetOnceMem.set_once_mem = SetOnceMem.@new(UNIT / exh)
        let cap : ThreadCapabilities.capability = ThreadCapabilities.@new(tag(future1GangSched), c / exh)
	return(cap)
      ;

      define @touch (fut : future / exh : PT.exh) : any =
        let tmp : any = CAS (&0(fut), EMPTY_F, EVAL_F)
        if Equal (tmp, EMPTY_F)
           then (* the future is ready for evaluation *)
             let result : any = @eval(fut / exh)
             let result : any = promote (result)
             do UPDATE(STATE_OFF, fut, result)
             return (result)
	else if Equal (tmp, STOLEN_F)
           then (* another fiber is evaluating the future; we need to block *)
                cont kLocal (_ : PT.unit) = 
                    (* resume the future *)
		     return (SELECT(STATE_OFF, fut))
                let kLocal : PT.fiber = (PT.fiber)kLocal
                (* make the future cancelable *)
                let kLocal : PT.fiber = C.@wrap(SELECT(CANCELABLE_OFF, fut), kLocal / exh)
                let k : PT.fiber = promote (kLocal)
   	        let tmpX : any = CAS (&0(fut), STOLEN_F, k)
 	        if Equal (tmpX, STOLEN_F)
	           then (* transfer control to the futures scheduler *)
                        Control.@stop (/ exh)
	          else (* the future value is ready *)
                       return (tmpX)
        else (* the future value is ready *)	       
            return (tmp)
	;

      define @future (thunk : thunk / exh : PT.exh) : future =
        let readyQ : LockedQueue.queue = @get-ready-queue(/ exh)
        let c : C.cancelable = C.@new(UNIT / exh)
        let fls : FLS.fls = FLS.@get(/ exh)
        let fut : future = alloc(EMPTY_F, thunk, c, fls)
        let fut : future = promote(fut)
        return(fut)
      ;
(*
    (* cancel a future and all of its descendants *)
      define @cancel (x : PT.unit / exh : PT.exh) : PT.unit =
	(* TODO *)
	return(UNIT)
      ;
*)
    )

    val touch : 'a future -> 'a = _prim(@touch)
    val future : 'a thunk -> 'a future = _prim(@future)
    val cancel : 'a future -> unit = _prim(@cancel)

  end
