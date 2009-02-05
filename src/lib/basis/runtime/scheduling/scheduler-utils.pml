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

      define @is-vp-idle (vp : vproc / exh : exh) : PT.bool =
	let isIdle : PT.bool = vpload(VP_IDLE, vp)
	return(isIdle)
      ;

      define @vproc-id (vp : vproc / exh : exh) : int =
	let id : int = vpload(VPROC_ID, vp)
	return(id)
      ;

      extern int GetNumVProcs ();

      define @num-vprocs (/ exh : exh) : int =
	let n : int = ccall GetNumVProcs()
	return(n)
      ;

      extern void *SleepCont (void *) __attribute__((alloc));

    (* put the vproc to sleep *)
      define @sleep (/ exh : exh) : () =
      (* resumption continuation for when the vproc awakens *)
	cont resumeK (x : unit) = return ()
      (* the C runtime expects the resumption continuation to be in vp->stdCont *)
	do vpstore(STD_CONT, host_vproc, resumeK)
      (* fiber that puts the vproc to sleep *)
	let sleepK : PT.fiber = ccall SleepCont (host_vproc)
	throw sleepK(UNIT)
      ;

    (* wait for a thread to reach the local vproc queue *)
      define @wait (/ exh : exh) : () =
        let m : PT.bool = vpload (ATOMIC, host_vproc)
        do vpstore(ATOMIC, host_vproc, true)
        do @sleep(/ exh)
        do Control.@handle-incoming(/ exh)
        do vpstore(ATOMIC, host_vproc, m)
        return()
      ;

      extern void *ListVProcs (void *) __attribute__((alloc));

      define @all-vprocs (/ exh : exh) : List.list =
	let vps : List.list = ccall ListVProcs(host_vproc)
        return(vps)
      ;

      define @other-vprocs (/ exh : exh) : List.list =
        fun lp (vps : List.list, others : List.list / exh : exh) : List.list =
	    case vps
	     of nil => return(others)
	      | List.CONS(vp : vproc, vps : List.list) =>
		if Equal(vp, host_vproc)
                   then apply lp(vps, others / exh)
		else apply lp(vps, List.CONS(vp, others) / exh)
	    end
	let vps : List.list = ccall ListVProcs(host_vproc)
	apply lp(vps, nil / exh)  
      ;

    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : () =
	fun lp (vps : List.list / exh : exh) : () =
	    case vps
	     of nil => return()
	      | List.CONS(vp : vproc, vps : List.list) =>
		do apply f(vp / exh)
		apply lp(vps / exh)
	    end
	let vps : List.list = ccall ListVProcs(host_vproc)
	apply lp(vps / exh)
      ;

    (* apply f to each vproc except the host *)
      define @for-other-vprocs(f : fun(vproc / exh ->) / exh : exh) : () =
        let self : vproc = host_vproc
        fun g (vp : vproc / exh : exh) : () =
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
      define @set-trampoline ( / exh : exh) : () =
        do vpstore(ATOMIC, host_vproc, true)

      (* the trampoline passes signals from the C runtime to the current scheduler. there are two possibilities:
       *  1. the vproc was awoken from an idle state
       *  2. a timer interrupt arrived
       *)
	cont trampoline (k : PT.fiber) = 
	  if Equal(k, M_NIL)
             then 
	      (* case 1 *)
	      Control.@forward(PT.STOP / exh)
	  else 
	      (* case 2 *)
	      Control.@forward(PT.PREEMPT(k) / exh)
	let trampoline : cont(PT.fiber) = promote(trampoline)

      (* set the trampoline on a given vproc *)
	fun setTrampoline (vp : vproc / exh : exh) : () =
	    let currentTrampoline : cont(PT.fiber) = vpload(VP_SCHED_CONT, vp)
	    do assert(Equal(currentTrampoline, nil))
	    do vpstore(VP_SCHED_CONT, vp, trampoline)
	    return()
	do @for-each-vproc(setTrampoline / exh)

        return()
      ;

    (* switch to a given thread *)
      define @switch-to (fls : FLS.fls, fiber : PT.fiber / exh : exh) noreturn =
	let _ : unit =  FLS.@set (fls / exh)
	do vpstore (ATOMIC, host_vproc, false)
	throw fiber (UNIT)
      ;

    (* switch to the next thread in the host vproc's ready queue *)
      define @dispatch (/ exh : exh) noreturn =
	let item : Option.option = VPQ.@dequeue(/ exh)
	cont lp () =
	  case item
	   of Option.NONE => 
	      do @sleep(/ exh)
	      throw lp()
	    | Option.SOME(qitem : VPQ.queue) =>
	      @switch-to (SELECT(FLS_OFF,qitem), SELECT(FIBER_OFF,qitem) / exh)
	  end
       throw lp()
      ;

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (unit / exh -> unit), fls : FLS.fls, dst : vproc / exh : exh) : () =
	cont fiber (x : unit) =
	  cont exh (exn : exn) = @dispatch ( / exh)
	  let (_ : unit) = apply f (UNIT / exh)
	  @dispatch ( / exh)
	do VPQ.@enqueue-on-vproc (dst, fls, fiber / exh)
	return ()
      ;

    (* bootstrap the default scheduler *)
      define @boot-default-scheduler (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
        let fls : FLS.fls = FLS.@get(/ exh)
	do @set-trampoline ( / exh)
	let self : vproc = host_vproc
      (* push the scheduler action on each remote vproc *)
	fun pushAct (vp : vproc / exh : exh) : () =
	    let act : PT.sched_act = apply mkAct (vp / exh)
            Control.@push-remote-act(vp, act / exh)
	cont startup (_ : unit) =
	  do vpstore(ATOMIC, self, true)
	(* install the scheduler on remote vprocs *)
	  do @for-other-vprocs(pushAct / exh)
        (* spawn a dummy thread to wake up the scheduler *)
	  cont dummyK (x : unit) = 
	       let _ : unit = Control.@stop(/ exh)
	       return()
	  fun init (vp : vproc / exh : exh) : () = 
	      VPQ.@enqueue-on-vproc(vp, fls, dummyK / exh)
	  do @for-other-vprocs(init / exh)
          do vpstore(ATOMIC, self, false)
	  return()
      (* make the scheduler instance for the host vproc *)
	let act : PT.sched_act = apply mkAct (host_vproc / exh)
	Control.@run-thread (act, startup, fls / exh)
      ;

    (* initialize a given scheduler on a collection of vprocs *)
      define @scheduler-startup (mkAct : fun (vproc / exh -> PT.sched_act), fls : FLS.fls, vps : List.list / exh : exh) : () =
	  let self : vproc = host_vproc
	  let nVProcs : int = PrimList.@length(vps / exh)
          let nVProcs : int = I32Sub (nVProcs, 1)
        (* this barrier ensures that scheduler instances start after all are initialized *)
	  let barrier : NWayBarrier.barrier = NWayBarrier.@new(nVProcs / exh)

	  fun init (_ : unit / exh : exh) : unit =	  
		do vpstore (ATOMIC, host_vproc, true)
	       (* this fiber synchronizes on the barrier and then exits to activate the scheduler *)
		cont dummyK (_ : unit) = 
		     do vpstore (ATOMIC, host_vproc, true)
                     do NWayBarrier.@ready(barrier / exh)
		     do NWayBarrier.@barrier(barrier / exh)
		     do Control.@forward (PT.STOP / exh)
		     return(UNIT)
	      (* make the scheduler instance for the host vproc *)
	       let act : PT.sched_act = apply mkAct (host_vproc / exh)
	       do Control.@run-thread (act, dummyK, fls / exh)
	       return(UNIT)

	  fun spawnOn (vp : vproc / exh : exh) : () = 
	      @spawn-on (init, fls, vp / exh)

	  cont startup (_ : unit) =
		do vpstore(ATOMIC, self, true)
	      (* install the scheduler on remote vprocs *)
		do @for-other-vprocs(spawnOn / exh)
		NWayBarrier.@barrier(barrier / exh)

	(* make the scheduler instance for the host vproc *)
	 let act : PT.sched_act = apply mkAct (host_vproc / exh)
	 do Control.@run-thread (act, startup, fls / exh)
	 return()
      ;

    )

  end
