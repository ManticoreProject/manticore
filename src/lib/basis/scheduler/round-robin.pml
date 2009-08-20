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
      define @round-robin (_ : unit / exh : exh) : unit = 

	fun mkSwitch (self : vproc / exh : exh) : PT.sched_act =
	    cont switch (s : PT.signal) =
	      cont dispatch () =
		fun enq (self : vproc, fls : FLS.fls, k : PT.fiber) : () = VProcQueue.@enqueue-from-atomic (self, fls, k)
		let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
		case item
		 of Option.NONE => 
		    do VProc.@sleep-from-atomic (self)
                    throw dispatch ()
		  | Option.SOME(qitem : VProcQueue.queue_item) =>
		    do SchedulerAction.@dispatch-from-atomic (self, switch, SELECT(FIBER_OFF, qitem), SELECT(FLS_OFF, qitem))
		    throw dispatch()
		end

	      case s
		of PT.STOP => 
		   throw dispatch ()
		 | PT.PREEMPT (k : PT.fiber) =>
		   let fls : FLS.fls = FLS.@get-in-atomic (self)
		   do VProcQueue.@enqueue-from-atomic (self, fls, k)
		   throw dispatch ()
(*		 | PT.SLEEP (k : PT.fiber, t : Time.time) =>
		   let fls : FLS.fls = FLS.@get-in-atomic (self)
		   do apply addToSleeping (t, fls, k)
		   throw dispatch()
*)
		 | _ =>
		   throw exh(Match)
	      end

            return (switch)

       (* run the scheduler on all vprocs *)
	do VProcInit.@bootstrap (mkSwitch / exh)
	return (UNIT)
      ;

    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = DEBUG("scheduler utils: initialized round-robin scheduler")

  end
