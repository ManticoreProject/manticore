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

    (* This subroutine provides low-level glue between our compiled scheduling code and our C runtime. Because 
     * asynchronous signals arrive first in the C runtime, we need a protocol for passing signals along to
     * compiled scheduling code. For this task we use a "trampoline," which in our case is a continuation 
     * that receives the suspended fiber that was interrupted by the signal. The trampoline packages the 
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
	  do VProc.@handle-messages(/ exh)
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
	    do vpstore(VP_SCHED_CONT, vp, trampoline)
	    return()
	do VProc.@for-each(setTrampoline / exh)
        return()
      ;

    (* switch to a given thread *)
      define @switch-to (fls : FLS.fls, fiber : PT.fiber / exh : exh) noreturn =
	let _ : PT.unit =  FLS.@set (fls / exh)
	do vpstore (ATOMIC, host_vproc, false)
	throw fiber (UNIT)
      ;

    (* switch to the next thread in the host vproc's ready queue *)
      define @dispatch (/ exh : exh) noreturn =
	let item : Option.option = VPQ.@dequeue(/ exh)
	cont lp () =
	  case item
	   of Option.NONE => 
	      do VProc.@sleep(/ exh)
	      throw lp()
	    | Option.SOME(qitem : VPQ.queue) =>
	      @switch-to (SELECT(FLS_OFF,qitem), SELECT(FIBER_OFF,qitem) / exh)
	  end
       throw lp()
      ;

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (PT.unit / exh -> PT.unit), fls : FLS.fls, dst : vproc / exh : exh) : () =
	cont fiber (x : PT.unit) =
	  cont exh (exn : exn) = @dispatch ( / exh)
	  let (_ : PT.unit) = apply f (UNIT / exh)
	  @dispatch ( / exh)
	do VPQ.@enqueue-on-vproc (dst, fls, fiber / exh)
	return ()
      ;

    (* push a copy of the top-level scheduler on each vproc (except the host)  *)
      define @seed-remote-action-stacks (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
	fun f (vp : vproc / exh : exh) : () =
	    let act : PT.sched_act = apply mkAct (vp / exh)
            Control.@push-remote-act(vp, act / exh)
	VProc.@for-others(f / exh)
      ;

    (* initialize scheduling code on each vproc (except the host). *)
      define @initialize-remote-schedulers (fls : FLS.fls / exh : exh) : () =
	let self : vproc = host_vproc
        do vpstore(ATOMIC, self, true)
	cont wakeupK (x : PT.unit) = 
	     let _ : PT.unit = Control.@stop(/ exh)
	     return()
	fun f (vp : vproc / exh : exh) : () = VPQ.@enqueue-on-vproc(vp, fls, wakeupK / exh)
	do VProc.@for-others(f / exh)
        do vpstore(ATOMIC, self, false)
        return()
      ;

    (* bootstrap the default scheduler *)
      define @boot-default-scheduler (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
        do vpstore(ATOMIC, host_vproc, true)
        let fls : FLS.fls = FLS.@get(/ exh)
	do @set-trampoline (/ exh)
        do @seed-remote-action-stacks(mkAct / exh)
	cont startLeadK (_ : PT.unit) = @initialize-remote-schedulers(fls / exh)
	let act : PT.sched_act = apply mkAct (host_vproc / exh)
	Control.@run(act, startLeadK / exh)
      ;

    (* initialize a given scheduler on a collection of vprocs *)
      define @scheduler-startup (mkAct : fun (vproc / exh -> PT.sched_act), fls : FLS.fls, vps : List.list / exh : exh) : () =
	  let self : vproc = host_vproc
          let nVProcs : int = VProc.@num-vprocs(/ exh)
          let nVProcs : int = I32Sub (nVProcs, 1)
        (* this barrier ensures that scheduler instances start after all are initialized *)
	  let barrier : Barrier.barrier = Barrier.@new(nVProcs / exh)

	  fun init (_ : PT.unit / exh : exh) : PT.unit =	  
		do vpstore (ATOMIC, host_vproc, true)
	       (* this fiber synchronizes on the barrier and then exits to activate the scheduler *)
		cont dummyK (_ : PT.unit) = 
		     do vpstore (ATOMIC, host_vproc, true)
                     do Barrier.@ready(barrier / exh)
		     do Barrier.@barrier(nVProcs, barrier / exh)
		     do Control.@forward (PT.STOP / exh)
		     return(UNIT)
	      (* make the scheduler instance for the host vproc *)
	       let act : PT.sched_act = apply mkAct (host_vproc / exh)
	       do Control.@run-thread (act, dummyK, fls / exh)
	       return(UNIT)

	  fun spawnOn (vp : vproc / exh : exh) : () = 
	      @spawn-on (init, fls, vp / exh)

	  cont startup (_ : PT.unit) =
		do vpstore(ATOMIC, self, true)
	      (* install the scheduler on remote vprocs *)
		do VProc.@for-others(spawnOn / exh)
		do Barrier.@barrier(nVProcs, barrier / exh)
                do vpstore (ATOMIC, host_vproc, false)
	        return()

	(* make the scheduler instance for the host vproc *)
	 let act : PT.sched_act = apply mkAct (host_vproc / exh)
	 do Control.@run-thread (act, startup, fls / exh)
	 return()
      ;

    )

  end
