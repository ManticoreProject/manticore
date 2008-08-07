(* scheduler-utils.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basic utilities for building schedulers:
 *  - connecting to the C runtime
 *  - thread spawning, switching, and dispatching operations
 *  - basic scheduler initialization
 *  - round-robin scheduler
 *)

structure SchedulerUtils =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    _primcode (

    (* this subroutine provides low-level glue between our compiled scheduling code and our C runtime. because 
     * asynchronous signals arrive first in the C runtime, we need a protocol for passing signals along to
     * compiled scheduling code. for this task we use a "trampoline," which in our case is a continuation 
     * that receives the suspended fiber that was interrupted by the signal. the trampoline packages the 
     * suspended continuation in a preemption signal and forwards it to the current scheduler.
     *
     * IMPORTANT: this operation must precede any scheduling operations, and signals must be masked before
     * this operation completes.
     *)
      define @set-trampoline (vps : List.list / exh : PT.exh) : () =
        do assert(Equal(vpload(ATOMIC, host_vproc), TRUE))

      (* to make this subroutine re-entrant, we ensure here that the trampoline is set just once *)
        cont exit () = return()
	let currentTrampoline : cont(PT.fiber) = vpload(VP_SCHED_CONT, host_vproc)
	do if NotEqual(currentTrampoline, NIL)
	      then throw exit()
	   else return()

      (* the trampoline forwards preemption signals to the current scheduler *)
	cont trampoline (k : PT.fiber) = Control.@forward(PT.PREEMPT(k) / exh)
	let trampoline : cont(PT.fiber) = promote(trampoline)

      (* set the trampoline on each vproc *)
	fun initVProcs (vps : List.list / exh : PT.exh) : () =
	    case vps
	     of NIL => 
		return()
	      | List.CONS (vp : vproc, vps : List.list) =>
		let currentTrampoline : cont(PT.fiber) = vpload(VP_SCHED_CONT, vp)
		do assert(Equal(currentTrampoline, NIL))
		do vpstore(VP_SCHED_CONT, vp, trampoline)
                apply initVProcs(vps / exh)
	    end
	do apply initVProcs(vps / exh)

        return()
      ;

    (* switch to a given thread *)
      define @switch-to (fls : FLS.fls, fiber : PT.fiber / exh : PT.exh) noreturn =
	let _ : PT.unit =  FLS.@set (fls / exh)
	do vpstore (ATOMIC, host_vproc, FALSE)
	throw fiber (UNIT)
      ;

    (* switch to the next thread in the host vproc's ready queue *)
      define @dispatch (/ exh : PT.exh) noreturn =
	  let qitem : VProcQueue.queue = VProcQueue.@dequeue ( / exh)
	  @switch-to (#0(qitem), #1(qitem) / exh)
      ;

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (PT.unit / PT.exh -> PT.unit), fls : FLS.fls, dst : vproc / exh : PT.exh) : () =
	cont fiber (x : PT.unit) =
	  cont exh (exn : exn) = @dispatch ( / exh)
	  let (_ : PT.unit) = apply f (UNIT / exh)
	  @dispatch ( / exh)
	do VProcQueue.@enqueue-on-vproc (dst, fls, fiber / exh)
	return ()
      ;

    (* initialize a given scheduler on  a collection of vprocs *)
      define @scheduler-startup (mkAct : fun (vproc / PT.exh -> PT.sigact), fls : FLS.fls, vps : List.list / exh : PT.exh) : () =
	(* bootstrap on the vprocs *)
	  do @set-trampoline (vps / exh)

	  let syncPoint : ![int] = alloc(0)
	 (* since vprocs share it, promote syncPoint *)
	  let syncPoint : ![int] = promote(syncPoint)

	  let self : vproc = host_vproc

	  let length : fun(List.list / PT.exh -> PT.ml_int) = pmlvar List.length
	  let nVProcs : PT.ml_int = apply length (vps / exh)
	 (* only count other vprocs *)
	  let nVProcs : int = I32Sub (#0(nVProcs), 1)

	 (* wait for all vprocs to have started the init function *)
	 fun spinWait (_ : PT.unit / exh : PT.exh) : () = 
	      let i : int = #0(syncPoint)     
	      if I32Eq (i, nVProcs) then	  
		do vpstore(ATOMIC, host_vproc, FALSE)
		return ()
	      else
		apply spinWait (UNIT / exh)

	 (* activate the scheduler *)
	  fun init (_ : PT.unit / exh : PT.exh) : PT.unit =	  
		do vpstore (ATOMIC, host_vproc, TRUE)
	       (* dummy fiber synchronizes on the barrier and then exits immediately to activate the scheduler *)
		cont dummyK (_ : PT.unit) = 
		     do vpstore (ATOMIC, host_vproc, TRUE)
		     let x : int = I32FetchAndAdd (syncPoint, 1)
		     do apply spinWait (UNIT / exh)
		     do Control.@forward (STOP / exh)
		     return(UNIT)
	      (* make the scheduler instance for the host vproc *)
	       let act : PT.sigact = apply mkAct (host_vproc / exh)
	       do Control.@run-thread (act, dummyK, fls / exh)
	       return(UNIT)

	 (* spawn the init function on the vprocs other than the initial vproc *)
	  fun spawnOnAll (vps : List.list / exh : PT.exh) : () =
		case vps
		 of NIL => return ()
		  | List.CONS (vp:vproc, rest:List.list) =>
		      if Equal(vp, self) then
                       (* do not spawn on the initial vproc *)
			apply spawnOnAll (rest / exh)
		      else
			do @spawn-on (init, fls, vp / exh)
			  apply spawnOnAll (rest / exh)
		end

	  cont startup (_ : PT.unit) =
		do vpstore(ATOMIC, self, TRUE)
	      (* install the scheduler on remote vprocs *)
		do apply spawnOnAll (vps / exh)
		apply spinWait (UNIT / exh)

	(* make the scheduler instance for the host vproc *)
	 let act : PT.sigact = apply mkAct (host_vproc / exh)
	 do Control.@run-thread (act, startup, fls / exh)
	 return()
      ;

      extern void *ListVProcs (void *) __attribute__((alloc));

    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin (x : PT.unit / exh : PT.exh) : PT.unit = 
	cont switch (s : PT.signal) =
	  let vp : vproc = host_vproc
	  let atomic : PT.bool = vpload(ATOMIC, vp)
	  do assert(atomic)

	  cont dispatch () =
	    let qitem : VProcQueue.queue = VProcQueue.@dequeue ( / exh)
	    do Control.@run-thread (switch, #1(qitem), #0(qitem) / exh)
            return(UNIT)

	  case s
	    of STOP => throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get ( / exh)
		 do VProcQueue.@enqueue (fls, k / exh)
		 throw dispatch () 
	  end

	fun mkSwitch (_ : vproc / exh : PT.exh) : PT.sigact = return (switch)

       (* get handles on all available vprocs  *)
	let vps : List.list = ccall ListVProcs(host_vproc)
       (* fiber-local storage for the top-level scheduler *)
	let fls : FLS.fls = FLS.@new (UNIT / exh)
       (* run the scheduler on all vprocs *)
	do @scheduler-startup (mkSwitch, fls, vps / exh)
	return (UNIT)
      ;

    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = Print.printLn "scheduler utils: initialized round-robin scheduler"

  end
