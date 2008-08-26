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
      define @pop-act (/ exh : PT.exh) : PT.sigact =
	let vp : vproc = host_vproc
	do vpstore(ATOMIC, vp, TRUE)
	let tos : [PT.sigact, any] = vpload(VP_ACTION_STK, vp)
	do assert(NotEqual(tos, NIL))
	let rest : any = #1(tos)
	do vpstore(VP_ACTION_STK, vp, rest)
	let act : PT.sigact = #0(tos)
	do assert (Equal(vp, host_vproc))
	return(act)
      ;

    (* push a scheduler action on a remote vproc's stack *)
      define @push-remote-act (vp : vproc, act : PT.sigact / exh : PT.exh) : () =
	do assert(NotEqual(act, NIL))
	let stk : [PT.sigact, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sigact, any] = alloc (act, (any)stk)
	let item : [PT.sigact, any] = promote (item)
	do vpstore (VP_ACTION_STK, vp, item)
        return()
      ;

    (* push a scheduler action on the host vproc's stack *)
      define @push-act (act : PT.sigact / exh : PT.exh) : () =
        let vp : vproc = host_vproc
	do vpstore (ATOMIC, vp, TRUE)
	do assert(NotEqual(act, NIL))
	let stk : [PT.sigact, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sigact, any] = alloc (act, (any)stk)
	do vpstore (VP_ACTION_STK, vp, item)
	do vpstore (ATOMIC, vp, FALSE)
        return()
      ;

    (* run the fiber under the scheduler action *)
      define @run (act : PT.sigact, fiber : PT.fiber / exh : PT.exh) noreturn =
        do @push-act(act / exh)
	throw fiber (UNIT)
      ;

    (* run a collection of fibers to completion and then return *)
      define @run-fibers (ks : List.list / exh : PT.exh) : () =
	cont lp (ks : List.list) = 
	  case ks
	   of NIL => return()
	    | List.CONS (k : PT.fiber, ks : List.list) =>
	       cont handler (sign : PT.signal) =
		 case sign
		  of STOP =>
		   (* handle the next messenger *)
		     throw lp(ks)
		   | PT.PREEMPT(k : PT.fiber) =>
		   (* ignore preemptions *)
		     @run(handler, k / exh)
		   | PT.UNBLOCK (retK : PT.fiber, k : PT.fiber, x : any) =>
		     throw lp(List.CONS(retK, List.CONS(k, ks)))
		 end
	      @run(handler, k / exh)	
	  end
	throw lp(ks)
      ;

    (* forward a signal to the host vproc  *)
      define @forward-no-check (sg : PT.signal / exh : PT.exh) noreturn =
        let act : PT.sigact = @pop-act(/ exh)
	throw act(sg)
      ;

    (* unload the landing pad, handle any incoming messages, and then forward a signal to the vproc *)
      define @forward (sg : PT.signal / exh : PT.exh) noreturn =
        do vpstore(ATOMIC, host_vproc, TRUE)
        let messages : List.list = VProcQueue.@unload-and-check-messages(/ exh)
        do @run-fibers(messages / exh)
        do vpstore(ATOMIC, host_vproc, TRUE)
        @forward-no-check(sg / exh)
      ;

    (* stop the current fiber *)
      define @stop (/ exh : PT.exh) : PT.unit =
        do @forward(STOP / exh)
        return(UNIT)
      ;

    (* yield control to the parent scheduler *)
      define @yield (/ exh : PT.exh) : PT.unit =
        cont k (x : PT.unit) = return(UNIT)
        do @forward(PT.PREEMPT(k) / exh)
        return(UNIT)
      ;

    (* yield control to the parent scheduler, masking signals upon return *)
      define @atomic-yield (/ exh : PT.exh) : PT.unit =
        cont k (x:PT.unit) = 
          do vpstore(ATOMIC, host_vproc, TRUE)         (* mask signals before resuming *)
          return(UNIT)
        do @forward(PT.PREEMPT(k) / exh)
        do assert(FALSE) (* control should never reach this point *)
        return(UNIT)
      ;

    (* unblock the given fiber with its associated data *)
      define @unblock (k : PT.fiber, x : any / exh : PT.exh) : PT.unit =
	cont retK (_ : PT.unit) = return(UNIT)
	do @forward(PT.UNBLOCK(retK, k, x) / exh)
	return(UNIT)
      ;

    (* run the thread under the scheduler action *)
      define @run-thread (act : PT.sigact, fiber : PT.fiber, fls : FLS.fls / exh : PT.exh) noreturn =
	do vpstore (ATOMIC, host_vproc, TRUE)
        let _ : PT.unit = FLS.@set(fls / exh)
        @run(act, fiber / exh)
      ;

    (* create a fiber *)
      define @fiber (f : PT.fiber_fun / exh : PT.exh) : PT.fiber =
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
