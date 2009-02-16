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
          cont dispatch () =
            let item : Option.option = VProcQueue.@dequeue-in-atomic(host_vproc)
            case item
	     of Option.NONE => 
		do VProc.@wait-in-atomic()
		throw dispatch()
	      | Option.SOME(qitem : VProcQueue.queue) =>
		do SchedulerAction.@dispatch-from-atomic (switch, #1(qitem), #0(qitem) / exh)
                return(UNIT)
            end

	  case s
	    of PT.STOP => 
	         throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get ( / exh)
		 do VProcQueue.@enqueue (fls, k)
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
    val _ = printMsg("scheduler utils: initialized round-robin scheduler")

  end
