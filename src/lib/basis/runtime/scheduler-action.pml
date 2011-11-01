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
      define inline @run (self : vproc, act : PT.sched_act, fiber : PT.fiber) noreturn;

    (* forward a signal to the host vproc  *)
      define inline @forward-from-atomic (sg : PT.signal) noreturn;
      define inline @forward (sg : PT.signal) noreturn;

    (* stop the current fiber; we assume that signals are masked *)
      define inline @stop-from-atomic (vp : vproc) noreturn =
    (* stop the current fiber *)
      define inline @stop () : unit;

    (* yield control to the parent scheduler *)
      define inline @yield-in-atomic (vp : vproc) : vproc;  
			                         (* returns the new host vproc *)
      define inline @yield-from-atomic (vp : vproc) : ();
      define inline @yield () : ();

    (* yeild control to the parent scheduler; indicate a voluntary release of the processor *)
      define inline @block-in-atomic (vp : vproc) : vproc;  
			                         (* returns the new host vproc *)
      define inline @block-from-atomic (vp : vproc) : ();
      define inline @block () : ();

    (* blocks the current fiber for a minimum of t nanoseconds *)
      define inline @sleep-in-atomic (vp : vproc, t : long) : ();  
			                         (* returns the new host vproc *)
      define inline @sleep-from-atomic (vp : vproc, t : long) : ();
      define inline @sleep (t : long) : ();
      define inline @sleepExport (t : [long]) : unit;

    (* create a fiber *)
      define inline @fiber (f : PT.fiber_fun / exh : exh) : PT.fiber;

    (* run the fiber under the scheduler action and with the given fls *)
      define inline @dispatch-from-atomic (self : vproc, act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls) noreturn;

    )

  end *) = struct

    structure PT = PrimTypes

    _primcode (

    (***** support for local atomicity *****)
      define inline @atomic-begin () : vproc =
	  let vp : vproc = host_vproc
        (* FIXME: we're hosed if these two surrounding instructions are
	 * not executed atomically, i.e., if a heap-limit check is inserted
	 * here. *)
	  do vpstore (ATOMIC, vp, true)
          do assert (Equal(vp, host_vproc))
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
	  do assert (Equal(vp, host_vproc))
	  return()
	;

    (* run the fiber under the scheduler action *)
      define inline @run (self : vproc, act : PT.sched_act, fiber : PT.fiber) noreturn =
	  do assert (Equal(self, host_vproc))
	  do @push-act(self, act)
	  do @atomic-end-no-check (self)
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

    (* stop the current fiber; we assume that signals are masked *)
      define inline @stop-from-atomic (vp : vproc) noreturn =
	  @forward-from-atomic (vp, PT.STOP)
	;

    (* stop the current fiber *)
      define inline @stop () noreturn =
	  @forward (PT.STOP)
	;

    (* yield control to the parent scheduler, masking signals upon return *)
      define inline @yield-in-atomic (vp : vproc) : vproc =
	  cont k (x : unit) = 
	    let vp : vproc = @atomic-begin()         (* mask signals before resuming *)
	    return(vp)
	  do @forward-from-atomic (vp, PT.PREEMPT(k))
	  do assert_fail() (* control should never reach this point *)
	  return(vp)
	;

    (* yield control to the parent scheduler *)
      define inline @yield-from-atomic (vp : vproc) : () =
	  cont k (x : unit) = return ()
	  do @forward-from-atomic (vp, PT.PREEMPT(k))
	  return ()
	;

    (* yield control to the parent scheduler *)
	define inline @yield () : () =
	  cont k (x : unit) = return ()
	  do @forward (PT.PREEMPT(k))
	  return ()
	;

    (* yield control to the parent scheduler, masking signals upon return; indicate voluntary release *)
      define inline @block-in-atomic (vp : vproc) : vproc =
	  cont k (x : unit) = 
	    let vp : vproc = @atomic-begin()         (* mask signals before resuming *)
	    return(vp)
	  do @forward-from-atomic (vp, PT.BLOCK(k))
	  do assert_fail() (* control should never reach this point *)
	  return(vp)
	;

    (* yield control to the parent scheduler; indicate voluntary release *)
      define inline @block-from-atomic (vp : vproc) : () =
	  cont k (x : unit) = return ()
	  do @forward-from-atomic (vp, PT.BLOCK(k))
	  return ()
	;

    (* yield control to the parent scheduler; indicate voluntary release *)
	define inline @block () : () =
	  cont k (x : unit) = return ()
	  do @forward (PT.BLOCK(k))
	  return ()
	;

    (* blocks the current fiber for a minimum of t nanoseconds *)
      define inline @sleep-in-atomic (vp : vproc, t : long) : vproc =
	  cont k (x : unit) = 
	    let vp : vproc = @atomic-begin()         (* mask signals before resuming *)
	    return(vp)
	  do @forward-from-atomic (vp, PT.SLEEP(k, t))
	  do assert_fail() (* control should never reach this point *)
	  return(vp)
	;

      define inline @sleep-from-atomic (vp : vproc, t : long) : () =
	  cont k (x : unit) = return ()
	  do @forward-from-atomic (vp, PT.SLEEP(k, t))
	  return ()
	;

      define inline @sleep (t : long) : () =
	  cont k (x : unit) = return ()
	  do @forward (PT.SLEEP(k, t))
	  return ()
	;

      define inline @sleepExport (t : [long] / _ : exh) : unit =
          let t1 : long = #0(t)
	  cont k (x : unit) = return (UNIT)
	  do @forward (PT.SLEEP(k, t1))
	  return (UNIT)
	;

    (* unmask signals; if there is a signal pending, then yield to the scheduler. *)
      define inline @atomic-end (vp : vproc) : () =
	  let pending : bool = vpload (SIG_PENDING, vp)
	    case pending
	     of true =>
		  do vpstore (SIG_PENDING, vp, false)
		  do @yield-from-atomic (vp)
		  return ()
	      | false =>
		  do @atomic-end-no-check (vp)
		  return ()
	    end
	;

    (* create a fiber *)
      define inline @fiber (f : PT.fiber_fun / exh : exh) : PT.fiber =
	  cont fiberK (x : unit) = 
	    let x : unit =
	    (* in case of an exception, just terminate the fiber *)
	      cont exh (exn : exn) = return (UNIT)
	      apply f (UNIT / exh)
	    let _ : unit = @stop (/ exh)
	    throw exh(tag(impossible))
	  return (fiberK)
	;

    (* run the fiber under the scheduler action and with the given fls *)
      define inline @dispatch-from-atomic (self : vproc, act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls) noreturn =
	  do assert (Equal(self, host_vproc))
	  do FLS.@set-in-atomic(self, fls)
	  @run (self, act, fiber)
	;

    )

    val sleep : long -> unit = _prim (@sleepExport)

  end
