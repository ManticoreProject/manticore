(* multiprogrammed-work-stealing.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Multiprogrammed work-stealing scheduler described by Arora et. al. (SPAA 1998).
 *)

structure MultiprogrammedWorkStealing :
  sig
    val workGroup : unit -> ImplicitThread.group
  end = struct

    _primcode(

      define @worker (id : int,                      (* worker id*)
		      nWorkers : int,                (* total number of workers in the group *)
                      deques : Arr.array		      
		     / exh : exh) : PT.fiber =

	let deque : DequeTHRep.deque = Arr.@sub(deques, id / exh)

	cont schedulerLoop (sign : PT.signal) =
	  cont dispatch (thd : ImplicitThread.thread) = 
	    do ImplicitThread.@run(schedulerLoop, thd / exh)
	    return($0)
       (* steal loop *)
	  cont steal () =
	    let victim : int = Rand.@in-range-int(0, nWorkers / exh)
	    do if I32Eq(victim, id)
		  then throw steal()
	       else return()
	    let victimDeque : DequeTHRep.deque = Arr.@sub(deques, victim / exh)
	    let thd : O.option = DequeTH.@pop-hd(victimDeque / exh)
	    case thd
	     of O.NONE => 
		let _ : PT.unit = Control.@atomic-yield( / exh)
		throw steal()
	      | O.SOME(thd : ImplicitThread.thread) =>
		 do assert(NotEqual(thd, enum(0)))
		 throw dispatch (thd)
	    end
        
	  case sign
	   of PT.STOP =>
	      let thd : O.option = DequeTh.@pop-tl(deque / exh)
	      case thd
	       of O.NONE => throw steal()
		| O.SOME(thd : ImplicitThread.thread) => throw dispatch(thd)
	      end
	    | PT.PREEMPT (k : PT.fiber) =>
	    (* mugging policy *)
	      let thd : ImplicitThread.thread = ImplicitThread.@capture(k / exh)
	      do DequeTH.@push-tl(deque, thd / exh)
	      let _ : PT.unit = Control.@atomic-yield(/exh)
	      throw schedulerLoop(PT.STOP)
	    | _ =>
	      let e : exn = Match
     	      throw exh(e)
	  end

        cont initK (x : unit) = throw schedulerLoop(PT.STOP)
	return(initK)
      ;

    (* create the work group *)
(*
      define @work-group (x : unit / exh : exh) : ImplicitThread.group =
        let nVPs : int = VProc.@num-vprocs(/ exh)
	let deques : Arr.array = Arr.@array(nVPs, enum(0) / exh)
	fun initDeque (vp : vproc / exh : exh) : () =
	    let id : int = VProc.@vproc-id(vp / exh)
	    let deque : DequeTH.deque = DequeTH.@new(/ exh)
	    Arr.@update(deques, id, deque / exh)
	do VProc.@for-each-vproc(initDeque / exh)
        fun mkAct (self : vproc / exh : exh) : PT.sched_act =
	    
	fun spawnFn (thd : ImplicitThread.thread / exh : exh) : unit =
            let id :
	    DequeTH.@push-tl(deque
	    return(UNIT)
	let group : ImplicitThread.group = ImplicitThread.@group(init, spawnFn / exh)
	return(group)
      ;

*)

    )

  end
