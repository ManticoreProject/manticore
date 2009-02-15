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

    (* run the fiber under the scheduler action *)
      define inline @run (act : PT.sched_act, fiber : PT.fiber / exh : exh) noreturn;
    (* forward a signal to the host vproc  *)
      define inline @forward (sg : PT.signal / exh : exh) noreturn;
    (* stop the current fiber *)
      define inline @stop (/ exh : exh) : unit;
    (* yield control to the parent scheduler *)
      define inline @yield (/ exh : exh) : unit;
    (* yield control to the parent scheduler. signals are masked upon return. *)
      define @atomic-yield (/ exh : exh) : unit;
    (* create a fiber *)
      define @fiber (f : PT.fiber_fun / exh : exh) : PT.fiber;
    (* run the fiber under the scheduler action and with the given fls *)
      define @run-with-fls (act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls / exh : exh) noreturn;

    )

  end *) = struct

    structure PT = PrimTypes

    _primcode (

    (* pop from the host vproc's scheduler action stack *)
      define @pop-act (/ exh : exh) : PT.sched_act =
	let vp : vproc = host_vproc
	do vpstore(ATOMIC, vp, true)
	let tos : [PT.sched_act, any] = vpload(VP_ACTION_STK, vp)
	do assert(NotEqual(tos, nil))
	let rest : any = #1(tos)
	do vpstore(VP_ACTION_STK, vp, rest)
	let act : PT.sched_act = #0(tos)
	do assert (Equal(vp, host_vproc))
	return(act)
      ;

    (* push a scheduler action on the host vproc's stack *)
      define @push-act (act : PT.sched_act / exh : exh) : () =
        let vp : vproc = host_vproc
	do vpstore (ATOMIC, vp, true)
	do assert(NotEqual(act, nil))
	let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sched_act, any] = alloc (act, (any)stk)
	do vpstore (VP_ACTION_STK, vp, item)
	do vpstore (ATOMIC, vp, false)
        return()
      ;

    (* run the fiber under the scheduler action *)
      define inline @run (act : PT.sched_act, fiber : PT.fiber / exh : exh) noreturn =
        do @push-act(act / exh)
	throw fiber (UNIT)
      ;

    (* forward a signal to the host vproc  *)
      define inline @forward (sg : PT.signal / exh : exh) noreturn =
        let act : PT.sched_act = @pop-act(/ exh)
	throw act(sg)
      ;

    (* stop the current fiber *)
      define inline @stop (/ exh : exh) : unit =
        do @forward(PT.STOP / exh)
        return(UNIT)
      ;

    (* yield control to the parent scheduler *)
      define inline @yield (/ exh : exh) : unit =
        cont k (x : unit) = return(UNIT)
        do @forward(PT.PREEMPT(k) / exh)
        return(UNIT)
      ;

    (* yield control to the parent scheduler, masking signals upon return *)
      define @atomic-yield (/ exh : exh) : unit =
        cont k (x:unit) = 
          do vpstore(ATOMIC, host_vproc, true)         (* mask signals before resuming *)
          return(UNIT)
        do @forward(PT.PREEMPT(k) / exh)
        do assert(false) (* control should never reach this point *)
        return(UNIT)
      ;

    (* create a fiber *)
      define @fiber (f : PT.fiber_fun / exh : exh) : PT.fiber =
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
      define @run-with-fls (act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls / exh : exh) noreturn =
	do vpstore (ATOMIC, host_vproc, true)
        let _ : unit = FLS.@set(fls / exh)
        @run(act, fiber / exh)
      ;

    )

  end
