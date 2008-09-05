(* prim-types.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimTypes =
  struct

    _primcode (

    (* exception handler *)
      typedef exh = cont(exn);
      typedef unit = enum(0);
      typedef fiber_fun = fun (unit / exh -> unit);
      typedef string_data = any;
      typedef ml_string = [string_data, int];
      typedef ml_int = [int];
      typedef ml_long = [long];
      typedef array = any;

    )

    type fiber = _prim ( cont(unit) )
    type 'a cont = _prim ( cont(any) )

    datatype bool = FALSE | TRUE

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
