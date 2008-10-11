(* work-stealing-sw-polling.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of the Work Stealing algorithm that uses software polling.
 *)

#define GLOBAL_HD_STOLEN    $0
#define GLOBAL_HD_EMPTY     $1

#define LOCAL_DEQUE_SZ      1024
#define LOCAL_DEQUE_HD      0
#define LOCAL_DEQUE_TL      1
#define LOCAL_DEQUE_ELTS    2

#define DUMMY_FIBER         $0

structure WorkStealingSWPolling =
  struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure FLS = FiberLocalStorage
    structure O = Option

    _primcode(

    (* globally visible head of the deque *)
      typedef global_hd = ![PT.fiber];
      typedef deque_elts = any;
      typedef local_deque = ![
                       int,                (* hd - points at the head fiber *)
		       int,                (* tl - points one above the tail fiber *)
		       deque_elts          (* array of fibers *)
                    ];

      extern void* M_GetLocalDeque (void*);
      extern void* GetNthVProc (int);
      extern void M_InitDeques (void*);

    (* number of threads in the local deque (assuming that the head was not stolen) *)
      define @local-deque-sz (localDeque : local_deque / exh : PT.exh) : int =
        return(I32Sub(SELECT(LOCAL_DEQUE_HD, localDeque), SELECT(LOCAL_DEQUE_TL, localDeque)))
      ;

    (* assume that if we pop the hd, we have exclusive access to it *)
      define @local-deque-pop-tl-only (localDeque : local_deque / exh : PT.exh) : PT.fiber =
        let elts : deque_elts = SELECT(LOCAL_DEQUE_ELTS, localDeque)
        let tl : int = I32Sub(SELECT(LOCAL_DEQUE_TL, localDeque), 1)
        let x : any = ArrayLoadI64(elts, tl)
        do UPDATE(LOCAL_DEQUE_TL, localDeque, tl)
        let b : PT.bool = ArrayStoreI64(elts, tl, nil)
        do assert(NotEqual(x, nil))
        return(x)
      ;

      define @local-deque-push-tl (localDeque : local_deque, elt : PT.fiber / exh : PT.exh) : () =
        let tl : int = SELECT(LOCAL_DEQUE_TL, localDeque)
        do UPDATE(LOCAL_DEQUE_TL, localDeque, I32Add(tl, 1))
        let x : PT.bool = ArrayStoreI64(SELECT(LOCAL_DEQUE_ELTS, localDeque), tl, elt)
        return()
      ;

    (* special case for when hd = tl *)
      define @pop-hd (localDeque : local_deque, globalHd : global_hd / exh : PT.exh) : O.option =
	fun lp () : O.option =
	    let globalHdV : PT.fiber = CAS(&0(globalHd), GLOBAL_HD_STOLEN, GLOBAL_HD_EMPTY)
	    if Equal(globalHdV, GLOBAL_HD_STOLEN)
	       then 
		return(O.NONE)
	    else if Equal(globalHdV, GLOBAL_HD_EMPTY)
	       then
	      (* broke our invariant: promote(hd(localDeque)) = #0(globalHd) *)
		do assert(PT.false)
		return(O.NONE)
	    else 
		let empty : PT.fiber = (PT.fiber)GLOBAL_HD_EMPTY
		let globalHdV' : PT.fiber = CAS(&0(globalHd), globalHdV, empty)
		if Equal(globalHdV, globalHdV')
		   then
		  (* we have a lock on the head, so use the top of the local deque *)
		    let hd : PT.fiber = @local-deque-pop-tl-only(localDeque / exh)
		    return(O.SOME(hd))
		else 
		  (* another worker stole the head *)
		    apply lp()
	apply lp()
      ;

    (* pop a thread from the local deque *)
      define @local-deque-pop-tl (globalHd : global_hd, localDeque : local_deque / exh : PT.exh) : O.option =
	let nThds : int = @local-deque-sz(localDeque / exh)
	if I32Eq(nThds, 1)
	   then
	    let hd : O.option = @pop-hd(localDeque, globalHd / exh)
	    return(hd)
	else if I32Gt(nThds, 1)
	   then
	    let k : PT.fiber = @local-deque-pop-tl-only(localDeque / exh)
	    return(O.SOME(k))
        else
	    do assert(PT.false)
            return(O.NONE)
      ;

    (* send a messenger that tries to move a thread from the local deque to the global head *)
      define @send-steal-req (vprocId : int, globalHd : global_hd / exh : PT.exh) : () =
      (* the thief copies the head of the local deque to the globally-visible head *)
        cont thiefK (x : PT.unit) =
          let localDeque : local_deque = ccall M_GetLocalDeque(host_vproc)
          let sz : int = @local-deque-sz(localDeque / exh)
        (* exit if there is nothing to steal *)
          do if I32Eq(sz, 0)
		then let _ : PT.unit = Control.@stop(/ exh)
                     return()
	     else return()
        (* exit if another thief has already copied the hd *)
          do if NotEqual(SELECT(0, globalHd), GLOBAL_HD_EMPTY)
	        then if NotEqual(SELECT(0, globalHd), GLOBAL_HD_STOLEN)
			then let _ : PT.unit = Control.@stop(/ exh)
                             return()
		     else return()
	     else return()
	(* copy the local hd to the global hd *)
	  let elts : deque_elts = SELECT(LOCAL_DEQUE_ELTS, localDeque)
	  let hd : PT.fiber = ArrayLoadI64(elts, SELECT(LOCAL_DEQUE_HD, localDeque))
	  let hd : PT.fiber = promote(hd)
	  do UPDATE(0, globalHd, hd)
	  return()
        let vp : vproc = ccall GetNthVProc(vprocId) 
        VProcMessenger.@send(vp, thiefK / exh)
      ;

    (* attempt to steal a thread  *)
      define @steal (globalHds : Arr.array / exh : PT.exh) : O.option =
      (* fix for the C preprocessor *)
        let stolen : PT.fiber = (PT.fiber)GLOBAL_HD_STOLEN
        let empty : PT.fiber = (PT.fiber)GLOBAL_HD_EMPTY

	cont exit () = return(O.NONE)
	let id : int = SchedulerUtils.@vproc-id(host_vproc / exh)
        let nWorkers : int = Arr.@length(globalHds / exh)
	let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	do if I32Eq(victim, id)
	      then throw exit()
	   else return()
	let victimGlblHd : global_hd = Arr.@sub(globalHds, victim / exh)
        let victimGlblHd : global_hd = (global_hd)victimGlblHd
	let stolenK : PT.fiber = SELECT(0, victimGlblHd)
	let stolenK' : PT.fiber = CAS(&0(victimGlblHd), stolenK, stolen)
	if NotEqual(stolenK, stolenK')
	   then return(O.NONE)
	else if Equal(stolenK', empty)
	   then 
	    do @send-steal-req(victim, victimGlblHd / exh)
	    return(O.NONE)
	else if Equal(stolenK', stolen)
	   then 
	    do @send-steal-req(victim, victimGlblHd / exh)
	    return(O.NONE)
	else return(O.SOME(stolenK'))
      ;

    (* create an instance of the scheduler for a vproc *)
      define @scheduler (globalHds : Arr.array, self : vproc / exh : PT.exh) : PT.sigact =
	cont error () = 
	  do assert(PT.false)
	  return($0)
	let nWorkers : int = Arr.@length(globalHds / exh)
	let id : int = SchedulerUtils.@vproc-id(self / exh)
	let globalHd : global_hd = Arr.@sub(globalHds, id / exh)
        let globalHd : global_hd = (global_hd)globalHd
	let localDeque : local_deque = ccall M_GetLocalDeque(self)

	cont switch (sign : PT.signal) =
        (* run a thread *)
	  cont run (switch : PT.sigact, k : PT.fiber) =
	    do Control.@run(switch, k / exh)
	    throw error()
        (* steal a thread from a remote vproc *)
	  cont steal (switch : PT.sigact) =
	    let kOpt : O.option = @steal(globalHds / exh)
	    case kOpt
	     of O.NONE => throw steal(switch)
	      | O.SOME (k : PT.fiber) => throw run(switch, k)
	    end
        (* handle a signal *)
	  case sign
	   of PT.STOP =>
	      let elt : O.option = @local-deque-pop-tl(globalHd, localDeque / exh)
	      case elt
	       of O.NONE => throw steal(switch)
		| O.SOME(k : PT.fiber) => throw run(switch, k)
	      end
	    | PT.PREEMPT (k : PT.fiber) =>
	      let _ : PT.unit = Control.@atomic-yield(/exh)
              throw run(switch, k)
	    | PT.SUSPEND (k : PT.fiber, retK : cont(PT.fiber)) =>
	      throw run(switch, k)
	    | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, x : Option.option) =>
	      do @local-deque-push-tl(localDeque, k / exh)
	      throw run(switch, retK)
          end

	return(switch)
      ;

      define @init (/ exh : PT.exh) : PT.unit =
      (* allocate local deques *)
	do ccall M_InitDeques(host_vproc)
      (* allocate globally visible deque heads *)
	let nVProcs : int = SchedulerUtils.@num-vprocs(/ exh)
	let globalHds : Arr.array = Arr.@array(nVProcs, enum(0) / exh)
        cont dummy (x : PT.unit) = 
          do assert(PT.false)
          return(UNIT)
	fun initGlobalHds (i : int / exh : PT.exh) : () =
	    if I32Gte(i, nVProcs)
	       then return()
	    else
		let hd : global_hd = alloc(dummy)
		let hd : global_hd = promote(hd)
		do Arr.@update(globalHds, i, hd / exh)
		apply initGlobalHds(I32Add(i, 1) / exh)
      (* initialize the scheduler*)
	let vps : List.list = SchedulerUtils.@all-vprocs(/ exh)
	let fls : FLS.fls = FLS.@get( / exh)
	fun mkAct (self : vproc / exh : PT.exh) : PT.sigact =
	      @scheduler(globalHds, self / exh)
	do SchedulerUtils.@scheduler-startup(mkAct, fls, vps / exh)  
	return(UNIT)
      ;

      define @get-global-hd (/ exh : PT.exh) : global_hd =
	let fls : FLS.fls = FLS.@get( / exh)
	let hds : Option.option = FLS.@find(fls, tag(workStealingSWGlobalHd) / exh)
        case hds
	 of O.NONE => 
	(* this thread does not support work stealing *)
        (* FIXME: throw an exception here *)
  	    do assert(PT.false)
	    return($0)
	  | O.SOME(hd : global_hd) =>
	    return(hd)
        end
      ;

      define @pop-tl(/ exh : PT.exh) : PT.bool =
	let localDeque : local_deque = ccall M_GetLocalDeque(host_vproc)
	let globalHd : global_hd = @get-global-hd(/ exh)
	let elt : O.option = @local-deque-pop-tl(globalHd, localDeque / exh)
	case elt
	 of O.NONE => return(PT.false)
	  | O.SOME(k : PT.fiber) => return(PT.true)
	end
      ;

      define @push-tl(k : PT.fiber / exh : PT.exh) : () =
        let localDeque : local_deque = ccall M_GetLocalDeque(host_vproc)
        @local-deque-push-tl(localDeque, k / exh)
      ;

    )

  end
