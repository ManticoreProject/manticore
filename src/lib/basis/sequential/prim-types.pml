(* prim-types.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * NOTE: most primtive types are defined in the initial-basis.pml file.
 *)

structure PrimTypes =
  struct

    _primcode (

    (* exception handler *)

      typedef array = any;
      typedef fiber_fun = fun (unit / exh -> unit);

    )

    type fiber = _prim ( cont(unit) )
    type 'a cont = _prim ( cont(any) )

    datatype bool = datatype bool

  (* signals for schedulers *)
    datatype 'a signal 
      = STOP                            (* terminate *)
      | PREEMPT of fiber                (* preempt the given fiber *)
      | UNBLOCK of (fiber *             (* return continuation for the fiber doing the unblocking *)
	            fiber *             (* fiber to unblock *)
		    'a)                 (* data associated with the unblocking fiber *)
      | SUSPEND of (fiber *             (* fiber to suspend *)
		    fiber cont)         (* return continuation *)
		  

  (* scheduler actions are continuations that consume a signal and perform a context switch *)
    type sigact = _prim ( cont(signal) )

  end
