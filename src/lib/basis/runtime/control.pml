(* control.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fiber control operations.
 *)

structure Control =
  struct

    structure PT = PrimTypes

    type fiber = _prim ( cont(PT.unit) )

    datatype signal = STOP | PREEMPT of fiber

    _primcode (
      typedef fiber_fun = fun (PT.unit / PT.exh -> PT.unit);

      define @forward (sg : signal /) noreturn =
        let vp : vproc = host_vproc
	do vpstore(ATOMIC, vp, TRUE)
	let tos : [sigact, any] = vpload(VP_ACTION_STK, vp)
      (*  do assert(NotEqual(tos, NIL))*)
	let rest : any = #1(tos)
	do vpstore(VP_ACTION_STK, vp, rest)
	let act : sigact = #0(tos)
      (* do assert (Equal(vp, host_vproc))*)
	throw act(sg)
      ;

      define @stop () : noreturn =
        @forward(STOP /)
      ;

      define @fiber (f : fiber_fun / exh : PT.exh) : fiber =
	cont fiberK (x : PT.unit) = 
	  let x : PT.unit =
	  (* in case of an exception, just terminate the fiber *)
	    cont exh (exn : PT.exn) = return (UNIT)
	      apply f (UNIT / exh)
	  @stop ()
	  return (fiberK)
      ;

    )

  end
