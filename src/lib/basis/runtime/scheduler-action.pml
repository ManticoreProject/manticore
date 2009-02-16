(* scheduler-action.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Scheduling operations that affect the scheduler-action stack.
 *)

structure SchedulerAction (* :
  sig
    _prim(

    (** support for local atomicity **)

      define inline @atomic-begin () : vproc;
      define inline @atomic-end-no-check (vp : vproc) : ();
      define inline @atomic-end (vp : vproc) : ();

    (* run the fiber under the scheduler action *)
      define inline @run (act : PT.sched_act, fiber : PT.fiber) noreturn;

    (* forward a signal to the host vproc  *)
      define inline @forward-from-atomic (sg : PT.signal) noreturn;
      define inline @forward (sg : PT.signal) noreturn;

    (* stop the current fiber *)
      define inline @stop () : unit;

    (* yield control to the parent scheduler *)
      define inline @yield-from-atomic () : unit;
      define inline @yield () : unit;
      define inline @yield-in-atomic (vp : vproc) : unit;

    (* create a fiber *)
      define inline @fiber (f : PT.fiber_fun / exh : exh) : PT.fiber;

    (* run the fiber under the scheduler action and with the given fls *)
      define inline @dispatch-from-atomic (act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls / exh : exh) noreturn;

    )

  end *) = struct

    structure PT = PrimTypes

    _primcode (

    (***** support for local atomicity *****)
      define inline @atomic-begin () : vproc =
	  let vp : vproc = host_vproc
	  do vpstore (ATOMIC, host_vproc, true)
	  return(vp)
	;

      define inline @atomic-end-no-check (vp : vproc) : () =
	  do vpstore (ATOMIC, vp, false)
	  return ()
	;

    (* pop from the host vproc's scheduler action stack *)
      define inline @pop-act (vp : vproc) : PT.sched_act =
	  let tos : [PT.sched_act, any] = vpload(VP_ACTION_STK, vp)
	  do assert (NotEqual(tos, nil))
	  let rest : any = #1(tos)
	  do vpstore(VP_ACTION_STK, vp, rest)
	  let act : PT.sched_act = #0(tos)
	  do assert (Equal(vp, host_vproc))
	  return(act)
	;

    (* push a scheduler action on the host vproc's stack *)
      define inline @push-act (vp : vproc, act : PT.sched_act) : () =
	  do assert(NotEqual(act, nil))
	  let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
	  let item : [PT.sched_act, any] = alloc (act, (any)stk)
	  do vpstore (VP_ACTION_STK, vp, item)
	  return()
	;

    (* run the fiber under the scheduler action *)
      define inline @run (act : PT.sched_act, fiber : PT.fiber) noreturn =
	  let vp : vproc = @atomic-begin ()
	  do @push-act(vp, act)
	  throw fiber (UNIT)
	;

    (* forward a signal to the host vproc; we assume that signals are masked *)
      define inline @forward-from-atomic (vp : vproc, sg : PT.signal) noreturn =
	  let act : PT.sched_act = @pop-act(vp)
	  throw act (sg)
	;

    (* forward a signal to the host vproc *)
      define inline @forward (sg : PT.signal) noreturn =
	  let vp : vproc = @atomic-begin ()
	  let act : PT.sched_act = @pop-act(vp)
	  throw act (sg)
	;

    (* stop the current fiber *)
      define inline @stop () : unit =
	  do @forward (PT.STOP)
	  return (UNIT)
	;

    (* yield control to the parent scheduler *)
      define inline @yield-from-atomic (vp : vproc) : unit =
	  cont k (x : unit) = return(UNIT)
	  do @forward-from-atomic (vp, PT.PREEMPT(k))
	  return (UNIT)
	;

    (* yield control to the parent scheduler *)
	define inline @yield () : unit =
	  cont k (x : unit) = return(UNIT)
	  do @forward (PT.PREEMPT(k))
	  return (UNIT)
	;

    (* yield control to the parent scheduler, masking signals upon return *)
      define inline @yield-in-atomic (vp : vproc) : unit =
	  cont k (x:unit) = 
	    let vp : vproc = @atomic-begin()         (* mask signals before resuming *)
	    return(UNIT)
	  do @forward-from-atomic (vp, PT.PREEMPT(k))
	  do assert(false) (* control should never reach this point *)
	  return(UNIT)
	;

    (* unmask signals; if there is a signal pending, then yield to the scheduler. *)
      define inline @atomic-end (vp : vproc) : () =
	  let pending : bool = vpload (SIG_PENDING, vp)
	    if pending
	      then
		do vpstore (SIG_PENDING, vp, false)
		let _ : unit = @yield-from-atomic (vp)
		return ()
	      else
		do @atomic-end-no-check (vp)
		return ()
	;

    (* create a fiber *)
      define inline @fiber (f : PT.fiber_fun / exh : exh) : PT.fiber =
	  cont fiberK (x : unit) = 
	    let x : unit =
	    (* in case of an exception, just terminate the fiber *)
	      cont exh (exn : PT.exn) = return (UNIT)
	      apply f (UNIT / exh)
	    let _ : unit = @stop (/ exh)
	    throw exh(tag(impossible))
	  return (fiberK)
	;

    (* run the fiber under the scheduler action and with the given fls *)
      define inline @dispatch-from-atomic (act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls / exh : exh) noreturn =
	  let _ : unit = FLS.@set(fls / exh)
	  @run (act, fiber)
	;

    )

  end
