(* round-robin.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RoundRobin =
  struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode(
    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin (x : unit / exh : exh) : unit = 
	cont switch (s : PT.signal) =
          let self : vproc = host_vproc

          cont dispatch () =
            let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
            case item
	     of Option.NONE => 
		do VProc.@sleep-from-atomic(self)
		throw dispatch()
	      | Option.SOME(qitem : VProcQueue.queue_item) =>
		do SchedulerAction.@dispatch-from-atomic (self, switch, SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem))
                return(UNIT)
            end

	  case s
	    of PT.STOP => 
	         throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get-in-atomic (self)
		 do VProcQueue.@enqueue-from-atomic (self, fls, k)
		 throw dispatch () 
	     | _ =>
	       let e : exn = Match
     	       throw exh(e)
	  end

	fun mkSwitch (_ : vproc / exh : PT.exh) : PT.sched_act = return (switch)

       (* run the scheduler on all vprocs *)
	do VProcInit.@bootstrap (mkSwitch / exh)
	return (UNIT)
      ;
    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = DEBUG("scheduler utils: initialized round-robin scheduler")

  end
