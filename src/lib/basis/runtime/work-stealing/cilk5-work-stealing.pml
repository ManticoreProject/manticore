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

    _primcode (

      define @scheduler (deques : Arr.array, self : vproc / exh : PT.exh) : PT.sigact =
	let nWorkers : int = Arr.@length(deques / exh)
	let id : int = SchedulerUtils.@vproc-id(self / exh)
	let deque : DequeTH.deque = Arr.@sub(deques, id / exh)
	cont switch (sign : PT.signal) =
	  cont dispatch (k : PT.fiber) = 
	    do Control.@run(switch, k / exh)
	    return($0)
	  cont steal () =
	    let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	    do if I32Eq(victim, id)
		  then throw steal()
	       else return()
	    let victimDeque : DequeTH.deque = Arr.@sub(deques, victim / exh)
	    let kOpt : Option.option = DequeTH.@pop-hd(victimDeque / exh)
	    case kOpt
	     of NONE => 
		let _ : PT.unit = Control.@atomic-yield( / exh)
		throw steal()
	      | Option.SOME(k : PT.fiber) =>
		 do assert(NotEqual(k, enum(0)))
		 throw dispatch (k)
	    end
	  case sign
	   of PT.STOP =>
	      let kOpt : Option.option = DequeTH.@pop-tl(deque / exh)
	      case kOpt
	       of NONE =>
		  throw steal()
		| Option.SOME(k : PT.fiber) =>
		  throw dispatch(k)
	      end
	    | PT.PREEMPT (k : PT.fiber) =>
	      do DequeTH.@push-tl(deque, k / exh)
	      let _ : PT.unit = Control.@atomic-yield(/exh)
	      throw switch(PT.STOP)
	    | PT.SUSPEND (k : PT.fiber, retK : cont(PT.fiber)) =>
	      let k' : PT.fiber = Control.@nested-sched-suspend(k, retK / exh)
              throw dispatch(k')
	    | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, x : Option.option) =>
	      let k : PT.fiber = 
		      case x
		       of NONE => 
			  return(k)
			| Option.SOME(c : Cancelation.cancelable) =>
			  let k : PT.fiber = Cancelation.@wrap(c, k / exh)
			  return(k)
		      end
	      do DequeTH.@push-tl(deque, k / exh)
	      throw dispatch(retK)
	  end

	return(switch)
      ;

      define @init (/ exh : PT.exh) : Arr.array =
	let fls : FLS.fls = FLS.@get( / exh)
	let nVPs : int = SchedulerUtils.@num-vprocs(/ exh)
	let deques : Arr.array = Arr.@array(nVPs, enum(0) / exh)
	fun f (vp : vproc / exh : PT.exh) : () =
	    let id : int = SchedulerUtils.@vproc-id(vp / exh)
	    let deque : DequeTH.deque = DequeTH.@new(/ exh)
	    Arr.@update(deques, id, deque / exh)
	do SchedulerUtils.@for-each-vproc(f / exh)
	fun mkAct (self : vproc / exh : PT.exh) : PT.sigact =
	      @scheduler(deques, self / exh)
	let vps : List.list = SchedulerUtils.@all-vprocs(/ exh)
	do SchedulerUtils.@scheduler-startup(mkAct, fls, vps / exh)
	return(deques)
      ;

    (* get the ith deque *)
      define @get-deque(i : int / exh : PT.exh) : DequeTH.deque =
	let fls : FLS.fls = FLS.@get( / exh)
	let deques : Option.option = FLS.@find(fls, tag(cilk5WorkStealing) / exh)
	let deques : Arr.array =
	     case deques
	       of NONE => 
		(* this thread does not support work stealing *)
	    (* FIXME: throw an exception here *)
		  do assert(PT.FALSE)
		  return($0)
		| Option.SOME (c : SetOnceMem.set_once_mem) =>
		  let deques : any = SetOnceMem.@get(c / exh)
		  return((Arr.array)deques)
	      end
	let deque : DequeTH.deque = Arr.@sub(deques, i / exh)
	return(deque)
      ;

      define @pop-tl ( / exh : PT.exh) : PT.bool =
	let id : int = SchedulerUtils.@vproc-id(host_vproc / exh)
	let deque : DequeTH.deque = @get-deque(id / exh)
	let kOpt : Option.option = DequeTH.@pop-tl(deque / exh)
	let isNonEmpty : PT.bool = 
	      case kOpt
	       of NONE => return (PT.FALSE)
		| Option.SOME(k : PT.fiber) => return(PT.TRUE)
	       end
	return(isNonEmpty)
      ;

      define @push-tl(k : PT.fiber / exh : PT.exh) : () =
	let id : int = SchedulerUtils.@vproc-id(host_vproc / exh)
	let deque : DequeTH.deque = @get-deque(id / exh)
	do DequeTH.@push-tl(deque, k / exh)
	return()
      ;

    )

  end
