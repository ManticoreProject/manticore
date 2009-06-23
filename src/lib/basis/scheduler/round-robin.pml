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

	cont switch (s : PT.signal) =
          let self : vproc = host_vproc

        let sleeping : ![List.list] = alloc(List.nil)
        let sleeping : ![List.list] = promote(sleeping)
        let nextWakeTime : ![Time.time] = alloc(0:long)
        let nextWakeTime : ![Time.time] = promote(nextWakeTime)

	fun addToSleepingList (timeToWake : Time.time, fls : FLS.fls, k : PT.fiber) : () =
	    let sleeping' : List.list = 
		    promote(List.CONS(alloc(timeToWake, fls, k), #0(sleeping)))
	    do #0(sleeping) := sleeping'
	    do if I64Lt (timeToWake, #0(nextWakeTime)) then
		   do #0(nextWakeTime) := timeToWake
		   return()
	       else if I64Eq (#0(nextWakeTime), 0:long) then
		   do #0(nextWakeTime) := timeToWake
		   return()
	       else
		   return()
	    return()

	fun unblockReadyThreads () : () =
	    fun doit (currTime : Time.time, sleeping : List.list) : () =
		case sleeping
		 of List.nil =>
		    return()
		  | List.CONS(item : [Time.time, FLS.fls, PT.fiber], sleeping : List.list) =>
		    if I64Gte (#0(item), currTime) then
			do VProcQueue.@enqueue-from-atomic (self, #1(item), #2(item))
			apply doit (currTime, sleeping)
		    else
			do apply addToSleepingList (#0(item), #1(item), #2(item))
			apply doit (currTime, sleeping)
		end
	    do #0(nextWakeTime) := 0:long
	    let currTime : Time.time = Time.@now (/ exh)
	    apply doit (currTime, #0(sleeping))

          cont dispatch () =
            do apply unblockReadyThreads()
            let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
            case item
	     of Option.NONE => 
		do if I64Eq (#0(nextWakeTime), 0:long) then
		       do VProc.@sleep-from-atomic(self)
                       return()
		   else
		       let sec : long = I64Div (#0(nextWakeTime), 1000000:long)
		       let nsec : long = I64Mul (I64Mod (#0(nextWakeTime), 1000000:long), 1000:long)
		       do VProc.@nanosleep-from-atomic(self, sec, nsec)
                       return()
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
	     | PT.SLEEP (k : PT.fiber, t : Time.time) =>
	       let fls : FLS.fls = FLS.@get-in-atomic (self)
               do apply addToSleepingList (t, fls, k)
               throw dispatch()
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
