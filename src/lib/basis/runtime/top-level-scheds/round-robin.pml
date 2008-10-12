(* round-robin.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure RoundRobin =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage
    structure VPQ = VProcQueue

    _primcode(
    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin (x : PT.unit / exh : PT.exh) : PT.unit = 
	cont switch (s : PT.signal) =
	  let vp : vproc = host_vproc
	  let atomic : PT.bool = vpload(ATOMIC, vp)
	  do assert(atomic)

          cont dispatch () =
            let item : Option.option = VPQ.@dequeue(/ exh)
            case item
	     of Option.NONE => 
		do SchedulerUtils.@wait(/ exh)
		throw dispatch()
	      | Option.SOME(qitem : VPQ.queue) =>
		do Control.@run-thread (switch, #1(qitem), #0(qitem) / exh)
                return(UNIT)
            end

	  case s
	    of PT.STOP => 
	         throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get ( / exh)
		 do VPQ.@enqueue (fls, k / exh)
		 throw dispatch () 
	     | PT.SUSPEND (k : PT.fiber, retK : PT.cont) =>
		 let fls : FLS.fls = FLS.@get ( / exh)
		 cont retK' (x : PT.unit) =
		   throw retK(k)
		 do VPQ.@enqueue (fls, k / exh)
		 throw dispatch () 
	     | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, fls : FLS.fls) =>
	         do VPQ.@enqueue (fls, k / exh)
		 do Control.@run(switch, retK / exh)
		 return(UNIT)
	  end

	fun mkSwitch (_ : vproc / exh : PT.exh) : PT.sigact = return (switch)

       (* run the scheduler on all vprocs *)
	do SchedulerUtils.@boot-default-scheduler (mkSwitch / exh)
	return (UNIT)
      ;
    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = printMsg("scheduler utils: initialized round-robin scheduler")

  end
