(* control.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Control operations for fibers.
 *)

structure Control =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    _primcode (

    (* pop from the host vproc's scheduler action stack *)
      define @pop-act (/ exh : PT.exh) : PT.sched_act =
	let vp : vproc = host_vproc
	do vpstore(ATOMIC, vp, PT.true)
	let tos : [PT.sched_act, any] = vpload(VP_ACTION_STK, vp)
	do assert(NotEqual(tos, nil))
	let rest : any = #1(tos)
	do vpstore(VP_ACTION_STK, vp, rest)
	let act : PT.sched_act = #0(tos)
	do assert (Equal(vp, host_vproc))
	return(act)
      ;

    (* push a scheduler action on a remote vproc's stack. NOTE: this operation is not
     * concurrent. the remote vproc must be idle during the operation.
     *)
      define @push-remote-act (vp : vproc, act : PT.sched_act / exh : PT.exh) : () =
	do assert(NotEqual(act, nil))
	let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sched_act, any] = alloc (act, (any)stk)
	let item : [PT.sched_act, any] = promote (item)
	do vpstore (VP_ACTION_STK, vp, item)
        return()
      ;

    (* push a scheduler action on the host vproc's stack *)
      define @push-act (act : PT.sched_act / exh : PT.exh) : () =
        let vp : vproc = host_vproc
	do vpstore (ATOMIC, vp, PT.true)
	do assert(NotEqual(act, nil))
	let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sched_act, any] = alloc (act, (any)stk)
	do vpstore (VP_ACTION_STK, vp, item)
	do vpstore (ATOMIC, vp, PT.false)
        return()
      ;

    (* run the fiber under the scheduler action *)
      define inline @run (act : PT.sched_act, fiber : PT.fiber / exh : PT.exh) noreturn =
        do @push-act(act / exh)
	throw fiber (UNIT)
      ;

    (* forward a signal to the host vproc  *)
      define inline @forward-no-check (sg : PT.signal / exh : PT.exh) noreturn =
        let act : PT.sched_act = @pop-act(/ exh)
	throw act(sg)
      ;

    (* run a collection of fibers to completion and then return *)
      define @run-fibers (ks : List.list / exh : PT.exh) : () =
	cont lp (ks : List.list) = 
	  case ks
	   of nil => return()
	    | List.CONS (k : PT.fiber, ks : List.list) =>
	       cont handler (sign : PT.signal) =
		 case sign
		  of PT.STOP =>
		   (* handle the next messenger *)
		     throw lp(ks)
		   | PT.PREEMPT(k : PT.fiber) =>
		   (* ignore preemptions *)
		     @run(handler, k / exh)
                   | _ =>
		     let e : exn = Match
                     throw exh (e)
		 end
	      @run(handler, k / exh)
	  end
	throw lp(ks)
      ;

      define @handle-incoming (/ exh : PT.exh) : () =
        let m : PT.bool = vpload(ATOMIC, host_vproc)
        do vpstore(ATOMIC, host_vproc, PT.true)
        let messages : List.list = VProcQueue.@unload-and-check-messages(/ exh)
        do @run-fibers(messages / exh)
        do vpstore(ATOMIC, host_vproc, m)
	return()
      ;

    (* unload the landing pad, handle any incoming messages, and then forward a signal to the vproc *)
      define inline @forward (sg : PT.signal / exh : PT.exh) noreturn =
        do @handle-incoming(/ exh)
        @forward-no-check(sg / exh)
      ;

    (* stop the current fiber *)
      define inline @stop (/ exh : PT.exh) : PT.unit =
        do @forward(PT.STOP / exh)
        return(UNIT)
      ;

    (* yield control to the parent scheduler *)
      define inline @yield (/ exh : PT.exh) : PT.unit =
        cont k (x : PT.unit) = return(UNIT)
        do @forward(PT.PREEMPT(k) / exh)
        return(UNIT)
      ;

    (* yield control to the parent scheduler, masking signals upon return *)
      define inline @atomic-yield (/ exh : PT.exh) : PT.unit =
        cont k (x:PT.unit) = 
          do vpstore(ATOMIC, host_vproc, PT.true)         (* mask signals before resuming *)
          return(UNIT)
        do @forward(PT.PREEMPT(k) / exh)
        do assert(PT.false) (* control should never reach this point *)
        return(UNIT)
      ;

    (* create a resumption fiber *)
      define inline @resume (k : PT.fiber, resK : cont(PT.fiber) / exh : PT.exh) : PT.fiber =
	cont resK' (x : PT.unit) =
	  throw resK(k)
	return(resK')
      ;

    (* run the thread under the scheduler action *)
      define inline @run-thread (act : PT.sched_act, fiber : PT.fiber, fls : FLS.fls / exh : PT.exh) noreturn =
	do vpstore (ATOMIC, host_vproc, PT.true)
        let _ : PT.unit = FLS.@set(fls / exh)
        @run(act, fiber / exh)
      ;

    (* create a fiber *)
      define inline @fiber (f : PT.fiber_fun / exh : PT.exh) : PT.fiber =
	cont fiberK (x : PT.unit) = 
	  let x : PT.unit =
	  (* in case of an exception, just terminate the fiber *)
	    cont exh (exn : PT.exn) = return (UNIT)
	    apply f (UNIT / exh)
	  let _ : PT.unit = @stop (/ exh)
          throw exh(tag(impossible))
	return (fiberK)
      ;

    )

  end
