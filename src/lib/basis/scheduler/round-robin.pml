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
	  (* sleeping threads sorted in increasing order by deadline *)
	    let sleeping : ![List.list] = alloc (List.nil)
	    let sleeping : ![List.list] = promote (sleeping)
	    fun addToSleeping (timeToWait : Time.time, fls : FLS.fls, k : PT.fiber) : () =
		fun insert (item : [Time.time, FLS.fls, PT.fiber], sleeping : List.list) : List.list =
		    case sleeping
		     of List.nil => 
			return (List.CONS (item, List.nil))
		      | List.CONS (item' : [Time.time, FLS.fls, PT.fiber], sleeping' : List.list) =>
			if I64Gt (#0(item'), #0(item)) then
			    return (List.CONS (item, sleeping))
			else
			    let sleeping'' : List.list = apply insert (item', sleeping')
			    return (List.CONS (item', sleeping''))
		     end
		let currTime : Time.time = Time.@now ()
		let sleeping' : List.list = 
			apply insert (alloc (I64Add (currTime, timeToWait), fls, k), #0(sleeping))
		let sleeping' : List.list = promote (sleeping')
		do #0(sleeping) := sleeping'
		return ()
	    fun removeFromSleeping (enq : fun (vproc, FLS.fls, PT.fiber / -> )) : () =
		let currTime : Time.time = Time.@now ()
		fun remove () : () =
		    case #0(sleeping)
		     of List.nil =>
			return ()
		      | List.CONS (item : [Time.time, FLS.fls, PT.fiber], sleeping' : List.list) =>
			if I64Gte (currTime, #0(item)) then
			    do #0(sleeping) := sleeping'
			    do apply enq (self, #1(item), #2(item))
			    apply remove ()
			else
			    return ()
		    end
		apply remove ()
          (* return the next point in time that one of the sleeping threads should wake up. we return a zero to
	   * indicate that there are no sleeping threads. *)
	    fun nextSleepingDeadline () : Time.time =
		case #0(sleeping)
		 of List.nil =>
		    return (0:long)
		  | List.CONS (item : [Time.time, FLS.fls, PT.fiber], sleeping' : List.list) =>
		    return (#0(item))
		end

	    cont switch (s : PT.signal) =
	      cont dispatch () =
		fun enq (self : vproc, fls : FLS.fls, k : PT.fiber) : () = VProcQueue.@enqueue-from-atomic (self, fls, k)
		do apply removeFromSleeping (enq)
		let item : Option.option = VProcQueue.@dequeue-from-atomic(self)
		case item
		 of Option.NONE => 
(* FIXME: figure out why vproc timeouts are causing cml benchmarks to freeze *)
throw dispatch ()
(*		    let currTime : Time.time = Time.@now ()
	            let nextSleepingDeadline : Time.time = apply nextSleepingDeadline ()
                    if I64Eq (nextSleepingDeadline, 0:long) then
			do VProc.@sleep-from-atomic (self)
			throw dispatch ()
		    else if I64Lt (nextSleepingDeadline, currTime) then
			throw dispatch ()
		    else
			let timeToSleepUsec : Time.time = I64Sub (nextSleepingDeadline, currTime)
			do VProc.@nanosleep-from-atomic (self, U64Mul (timeToSleepUsec, 1000:long))
			throw dispatch ()
*)
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
		 | PT.SLEEP (k : PT.fiber, t : Time.time) =>
		   let fls : FLS.fls = FLS.@get-in-atomic (self)
		   do apply addToSleeping (t, fls, k)
		   throw dispatch()
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
