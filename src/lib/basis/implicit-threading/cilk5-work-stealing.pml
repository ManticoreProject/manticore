(* cilk5-work-stealing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Cilk-5 implementation of the Work Stealing algorithm.
 *)

structure Cilk5WorkStealing =
  struct

    structure PT = PrimTypes
    structure Arr = Array64
    structure FLS = FiberLocalStorage
    structure O = Option

    _primcode (

    (* construct a single worker for a work-stealing group *)
      define @scheduler (deques : Arr.array, self : vproc / exh : exh) : PT.sched_act =

	let nWorkers : int = Arr.@length(deques / exh)
	let id : int = SchedulerUtils.@vproc-id(self / exh)
	let deque : DequeTH.deque = Arr.@sub(deques, id / exh)
      (* scheduler action for work stealing *)
	cont switch (sign : PT.signal) =
	  cont dispatch (k : PT.fiber) = 
	    do Control.@run(switch, k / exh)
	    return($0)
       (* steal a thread *)
	  cont steal () =
	    let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	    do if I32Eq(victim, id)
		  then throw steal()
	       else return()
	    let victimDeque : DequeTH.deque = Arr.@sub(deques, victim / exh)
	    let kOpt : O.option = DequeTH.@pop-hd(victimDeque / exh)
	    case kOpt
	     of O.NONE => 
		let _ : PT.unit = Control.@atomic-yield( / exh)
		throw steal()
	      | O.SOME(k : PT.fiber) =>
		 do assert(NotEqual(k, enum(0)))
		 throw dispatch (k)
	    end
        
	  case sign
	   of PT.STOP =>
	      let kOpt : O.option = DequeTH.@pop-tl(deque / exh)
	      case kOpt
	       of O.NONE => throw steal()
		| O.SOME(k : PT.fiber) => throw dispatch(k)
	      end
	    | PT.PREEMPT (k : PT.fiber) =>
	    (* mugging policy *)
	      do DequeTH.@push-tl(deque, k / exh)
	      let _ : PT.unit = Control.@atomic-yield(/exh)
	      throw switch(PT.STOP)
	    | _ =>
	      let e : exn = Match
     	      throw exh(e)
	  end

	return(switch)
      ;

    (* get the ith deque *)
      define @get-local-deque( / exh : exh) : DequeTH.deque =
        let deques : any = ThreadCapabilities.@get-from-fls(tag(cilk5WorkStealing) / exh)
	let id : int = SchedulerUtils.@vproc-id(host_vproc / exh)
	let deque : DequeTH.deque = Arr.@sub(deques, id / exh)
	return(deque)
      ;

      define @pop-tl ( / exh : exh) : bool =
	let deque : DequeTH.deque = @get-local-deque( / exh)
	let kOpt : O.option = DequeTH.@pop-tl(deque / exh)
	let isNonEmpty : bool = 
	      case kOpt
	       of O.NONE => return (false)
		| O.SOME(k : PT.fiber) => return(true)
	       end
	return(isNonEmpty)
      ;

      define @push-tl(k : PT.fiber / exh : exh) : () =
	let deque : DequeTH.deque = @get-local-deque( / exh)
	do DequeTH.@push-tl(deque, k / exh)
	return()
      ;

    (* initialize the work-stealing scheduler *)
      define @init (/ exh : exh) : Arr.array =
	let fls : FLS.fls = FLS.@get( / exh)
	let nVPs : int = SchedulerUtils.@num-vprocs(/ exh)
	let deques : Arr.array = Arr.@array(nVPs, enum(0) / exh)
	fun initDeque (vp : vproc / exh : exh) : () =
	    let id : int = SchedulerUtils.@vproc-id(vp / exh)
	    let deque : DequeTH.deque = DequeTH.@new(/ exh)
	    Arr.@update(deques, id, deque / exh)
	do SchedulerUtils.@for-each-vproc(initDeque / exh)
	fun mkAct (self : vproc / exh : exh) : PT.sched_act =
	      @scheduler(deques, self / exh)
	let vps : List.list = SchedulerUtils.@all-vprocs()
	do SchedulerUtils.@scheduler-startup(mkAct, fls, vps / exh)
	return(deques)
      ;

    )

  end
