(* work-stealing-sw-polling.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of the Work Stealing algorithm that uses software polling.
 *)

structure WorkStealingSWPolling =
  struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure FLS = FiberLocalStorage

    #define GLOBAL_HD_STOLEN    $0
    #define GLOBAL_HD_EMPTY     $1

    #define LOCAL_DEQUE_SZ      2048
    #define EMPTY_K             enum(0)

    _primcode(

      typedef global_hd = ![PT.fiber];
      typedef local_deque = ![int, int, Arr.array];

      define @local-deque-sub (localDeque : local_deque, i : int / exh : PT.exh) : PT.fiber =
        
      ;

      define @local-deque-sz (localDeque : local_deque / exh) : int

      ;

    (* send a messenger that tries to move a thread from the local deque to the global head *)
      define @send-steal-req (vprocId : int / exh : PT.exh) : () =

      ;

    (* special case for accessing the top of the deque *)
      define @get-local-deque-top (localDeque : local_deque, globalHd : global_hd / exh : PT.exh) : O.option =
	fun lp () : () =
	    let globalHdV : PT.fiber = CAS(&0(globalHd), GLOBAL_HD_STOLEN, GLOBAL_HD_EMPTY)
	    if Equal(globalHdV, GLOBAL_HD_STOLEN)
	       then 
		return(O.NONE)
	    else if Equal(globalHdVal, GLOBAL_HD_EMPTY)
	       then
	      (* broke our invariant: promote(hd(localDeque)) = #0(globalHd) *)
		do assert(PT.FALSE)
		return(NONE)
	    else 
		let globalHdV' : PT.fiber = CAS(&0(globalHd), globalHdV, GLOBAL_HD_EMPTY)
		if Equal(globalHdV, globalHdV')
		   then
		  (* we have a lock on the head *)
		    let hd : PT.fiber = @local-deque-sub(localDeque, SELECT(LOCAL_DEQUE_HD, localDeque) / exh)
		    return(O.SOME(hd))
		else 
		  (* another worker stole the head *)
		    apply lp()
	apply lp()
      ;

    (* attempt to steal a thread  *)
      define @steal (globalHds : Arr.array / exh : PT.exh) : O.option =
	cont exit () = return(O.NONE)
	let id : int = SchedulerUtils.@vproc-id(host_vproc / exh)
	let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	do if I32Eq(victim, id)
	      then throw exit()
	   else return()
	let victimDeque : global_hd = Arr.@sub(globalHds, victim / exh)
	let victimDequeV : PT.fiber = SELECT(0, victimDeque)
	let victimDequeV' : PT.fiber = CAS(&0(victimHd), victimDequeV, GLOBAL_HD_STOLEN)
	if NotEqual(victimDequeV, victimDequeV')
	   then return(O.NONE)
	else if Equal(victimDequeV, GLOBAL_HD_EMPTY)
	   then 
	    do @send-steal-req(victim / exh)
	    return(O.NONE)
	else return(O.SOME(victimDequeV'))
      ;

    (* create an instance of the scheduler for a vproc *)
      define @scheduler (globalHds : Arr.array, self : vproc / exh : PT.exh) : PT.sigact =
	cont error () = 
	  do assert(PT.FALSE)
	  return(enum(0):PT.sigact)
	let nWorkers : int = Arr.@length(deques / exh)
	let id : int = SchedulerUtils.@vproc-id(self / exh)
	let globalHd : global_hd = (global_hd)Arr.@sub(globalHds, id / exh)
	let localDeque : local_deque = @new-local-deque(/ exh)

	cont run (switch : PT.sigact, k : PT.fiber) =
	  do Control.@run(switch, k / exh)
	  throw error()

	cont steal (switch : PT.sigact) =
	  let kOpt : O.option = @steal(globalHds / exh)
          case kOpt
	   of O.NONE => throw steal()
	    | O.SOME(k : PT.fiber) => throw run(switch, k)
          end

	cont switch (sign : PT.signal) =
	  case sign
	   of PT.STOP =>
	      let nThds : int = @local-deque-sz(localDeque / exh)
	      if I32Eq(nThds, 1)
		 then
		  let hd : O.option = @get-local-deque-top(localDeque, globalHd / exh)
		  case hd
		   of O.NONE => throw steal(switch)
		    | O.SOME(k : PT.fiber) => throw run(switch, k)
		  end
	      else if I32Gt(nThds, 1)
		 then
		  let k : PT.fiber = @local-deque-tl(localDeque / exh)
		  throw run(switch, k)
	      else throw steal(switch)
	return(switch)
      ;

    )

  end
