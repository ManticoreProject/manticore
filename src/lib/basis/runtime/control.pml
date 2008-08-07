(* control.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Control operations for fibers.
 *)

#include "../misc/assert.def"

structure Control =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    _primcode (

    (* forward a signal to the vproc *)
      define @forward (sg : PT.signal / exh : PT.exh) noreturn =
        let vp : vproc = host_vproc
	do vpstore(ATOMIC, vp, TRUE)
	let tos : [PT.sigact, any] = vpload(VP_ACTION_STK, vp)
        do assert(NotEqual(tos, NIL))
	let rest : any = #1(tos)
	do vpstore(VP_ACTION_STK, vp, rest)
	let act : PT.sigact = #0(tos)
        do assert (Equal(vp, host_vproc))
	throw act(sg)
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

    (* run the fiber under the scheduler action *)
      define @run (act : PT.sigact, fiber : PT.fiber / exh : PT.exh) noreturn =
        let vp : vproc = host_vproc
	do vpstore (ATOMIC, vp, TRUE)
        do assert(NotEqual(act, NIL))
	let stk : [PT.sigact, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sigact, any] = alloc (act, (any)stk)
	do vpstore (VP_ACTION_STK, vp, item)
	do vpstore (ATOMIC, vp, FALSE)
	throw fiber (UNIT)
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
