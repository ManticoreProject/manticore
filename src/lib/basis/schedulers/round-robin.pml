(* round-robin.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RoundRobin =
  struct

    structure PT = PrimTypes

    _primcode(
    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin (x : unit / exh : exh) : unit = 
	cont switch (s : PT.signal) =
          let self : vproc = host_vproc

          cont dispatch () =
            let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
            case item
	     of Option.NONE => 
(* choose whether the vproc goes to sleep when there is no work to do *)
#if 0
		do VProc.@wait-in-atomic()
#endif
		throw dispatch()
	      | Option.SOME(qitem : VProcQueue.queue) =>
		do SchedulerAction.@dispatch-from-atomic (self, switch, #1(qitem), #0(qitem))
                return(UNIT)
            end

	  case s
	    of PT.STOP => 
	         throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get ()
		 do VProcQueue.@enqueue-from-atomic (self, fls, k)
		 throw dispatch () 
	     | _ =>
	       let e : exn = Match
     	       throw exh(e)
	  end

	fun mkSwitch (_ : vproc / exh : PT.exh) : PT.sched_act = return (switch)

       (* run the scheduler on all vprocs *)
	do VProc.@boot-default-scheduler (mkSwitch / exh)
	return (UNIT)
      ;
    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = DEBUG("scheduler utils: initialized round-robin scheduler")

  end
