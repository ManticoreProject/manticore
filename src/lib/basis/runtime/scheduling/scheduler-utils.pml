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

#include "vproc-queue.def"

structure SchedulerUtils =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage
    structure VPQ = VProcQueue

    _primcode (

      extern void *SleepCont (void *) __attribute__((alloc));

    (* put the vproc to sleep *)
      define @sleep (/ exh : PT.exh) : () =
      (* resumption continuation for when the vproc awakens*)
	cont resumeK (x : PT.unit) = return ()
      (* the C runtime expects the resumption continuation to be in vp->stdCont *)
	do vpstore(STD_CONT, host_vproc, resumeK)
      (* fiber that puts the vproc to sleep *)
	let sleepK : PT.fiber = ccall SleepCont (host_vproc)
	throw sleepK(UNIT)
      ;

      extern void *ListVProcs (void *) __attribute__((alloc));

    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / PT.exh ->) / exh : PT.exh) : () =
	fun lp (vps : List.list / exh : PT.exh) : () =
	    case vps
	     of NIL => return()
	      | List.CONS(vp : vproc, vps : List.list) =>
		do apply f(vp / exh)
		apply lp(vps / exh)
	    end
	let vps : List.list = ccall ListVProcs(host_vproc)
	apply lp(vps / exh)
      ;

    (* apply f to each vproc except the host *)
      define @for-other-vprocs(f : fun(vproc / PT.exh ->) / exh : PT.exh) : () =
        let self : vproc = host_vproc
        fun g (vp : vproc / exh : PT.exh) : () =
	    if NotEqual(vp, self)
	       then apply f(vp / exh)
	    else return()
        @for-each-vproc(g / exh)
      ;

    (* this subroutine provides low-level glue between our compiled scheduling code and our C runtime. because 
     * asynchronous signals arrive first in the C runtime, we need a protocol for passing signals along to
     * compiled scheduling code. for this task we use a "trampoline," which in our case is a continuation 
     * that receives the suspended fiber that was interrupted by the signal. the trampoline packages the 
     * suspended continuation in a preemption signal and forwards it to the current scheduler.
     *
     * IMPORTANT: this operation must precede any scheduling operations, and signals must be masked before
     * this operation completes.
     *)
      define @set-trampoline ( / exh : PT.exh) : () =
        do vpstore(ATOMIC, host_vproc, TRUE)

      (* the trampoline passes signals from the C runtime to the current scheduler. there are two possibilities:
       *  1. the vproc was awoken from an idle state
       *  2. a timer interrupt arrived
       *)
	cont trampoline (k : PT.fiber) = 
	  if Equal(k, M_NIL)
             then 
	      (* case 1 *)
	      Control.@forward(STOP / exh)
	  else 
	      (* case 2 *)
	      Control.@forward(PT.PREEMPT(k) / exh)
	let trampoline : cont(PT.fiber) = promote(trampoline)

      (* set the trampoline on a given vproc *)
	fun setTrampoline (vp : vproc / exh : PT.exh) : () =
	    let currentTrampoline : cont(PT.fiber) = vpload(VP_SCHED_CONT, vp)
	    do assert(Equal(currentTrampoline, NIL))
	    do vpstore(VP_SCHED_CONT, vp, trampoline)
	    return()
	do @for-each-vproc(setTrampoline / exh)

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
	let item : Option.option = VPQ.@dequeue(/ exh)
	cont lp () =
	  case item
	   of NONE => 
	      do @sleep(/ exh)
	      throw lp()
	    | Option.SOME(qitem : VPQ.queue) =>
	      @switch-to (SELECT(FLS_OFF,qitem), SELECT(FIBER_OFF,qitem) / exh)
	  end
       throw lp()
      ;

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (PT.unit / PT.exh -> PT.unit), fls : FLS.fls, dst : vproc / exh : PT.exh) : () =
	cont fiber (x : PT.unit) =
	  cont exh (exn : exn) = @dispatch ( / exh)
	  let (_ : PT.unit) = apply f (UNIT / exh)
	  @dispatch ( / exh)
	do VPQ.@enqueue-on-vproc (dst, fls, fiber / exh)
	return ()
      ;

    (* bootstrap the default scheduler *)
      define @boot-default-scheduler (mkAct : fun (vproc / PT.exh -> PT.sigact), fls : FLS.fls / exh : PT.exh) : () =
	do @set-trampoline ( / exh)
	let self : vproc = host_vproc
      (* push the scheduler action on each remote vproc *)
	fun pushAct (vp : vproc / exh : PT.exh) : () =
	    let act : PT.sigact = apply mkAct (vp / exh)
            Control.@push-remote-act(vp, act / exh)

	  cont startup (_ : PT.unit) =
		do vpstore(ATOMIC, self, TRUE)
	      (* install the scheduler on remote vprocs *)
		do @for-other-vprocs(pushAct / exh)
		return()
	(* make the scheduler instance for the host vproc *)
	 let act : PT.sigact = apply mkAct (host_vproc / exh)
	 Control.@run-thread (act, startup, fls / exh)
      ;

    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin (x : PT.unit / exh : PT.exh) : PT.unit = 
	cont switch (s : PT.signal) =
	  let vp : vproc = host_vproc
	  let atomic : PT.bool = vpload(ATOMIC, vp)
	  do assert(atomic)

          cont dispatch () =
            let item : Option.option = VPQ.@dequeue(/ exh)
            case item
	     of NONE => 
		do @sleep(/ exh)
		throw dispatch()
	      | Option.SOME(qitem : VPQ.queue) =>
		do Control.@run-thread (switch, #1(qitem), #0(qitem) / exh)
                return(UNIT)
            end

	  case s
	    of STOP => throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get ( / exh)
		 do VProcQueue.@enqueue (fls, k / exh)
		 throw dispatch () 
	  end

	fun mkSwitch (_ : vproc / exh : PT.exh) : PT.sigact = return (switch)

       (* fiber-local storage for the top-level scheduler *)
	let fls : FLS.fls = FLS.@new (UNIT / exh)
       (* run the scheduler on all vprocs *)
	do @boot-default-scheduler (mkSwitch, fls / exh)
	return (UNIT)
      ;

    (* initialize a given scheduler on a collection of vprocs *)
      define @scheduler-startup (mkAct : fun (vproc / PT.exh -> PT.sigact), fls : FLS.fls, vps : List.list / exh : PT.exh) : () =
	  let self : vproc = host_vproc
	  let length : fun(List.list / PT.exh -> PT.ml_int) = pmlvar List.length
	  let nVProcs : PT.ml_int = apply length (vps / exh)
          let nVProcs : int = I32Sub (#0(nVProcs), 1)
        (* this barrier ensures that scheduler instances start after all are initialized *)
	  let barrier : Barrier.barrier = Barrier.@new(nVProcs / exh)

	  fun init (_ : PT.unit / exh : PT.exh) : PT.unit =	  
		do vpstore (ATOMIC, host_vproc, TRUE)
	       (* this fiber synchronizes on the barrier and then exits to activate the scheduler *)
		cont dummyK (_ : PT.unit) = 
		     do vpstore (ATOMIC, host_vproc, TRUE)
                     do Barrier.@ready(barrier / exh)
		     do Barrier.@barrier(nVProcs, barrier / exh)
                     do vpstore(ATOMIC, host_vproc, FALSE)
		     do Control.@forward (STOP / exh)
		     return(UNIT)
	      (* make the scheduler instance for the host vproc *)
	       let act : PT.sigact = apply mkAct (host_vproc / exh)
	       do Control.@run-thread (act, dummyK, fls / exh)
	       return(UNIT)

	  fun spawnOn (vp : vproc / exh : PT.exh) : () = 
	      @spawn-on (init, fls, vp / exh)

	  cont startup (_ : PT.unit) =
		do vpstore(ATOMIC, self, TRUE)
	      (* install the scheduler on remote vprocs *)
		do @for-other-vprocs(spawnOn / exh)
		Barrier.@barrier(nVProcs, barrier / exh)

	(* make the scheduler instance for the host vproc *)
	 let act : PT.sigact = apply mkAct (host_vproc / exh)
	 do Control.@run-thread (act, startup, fls / exh)
	 return()
      ;

    )

    val roundRobin : unit -> unit = _prim (@round-robin)
    val _ = roundRobin()
    val _ = Print.printLn "scheduler utils: initialized round-robin scheduler"

  end
