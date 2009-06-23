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

    (* mutable 1-D arrays *)
      typedef array = [
                ![any],               (* array data *)
                int                   (* number of elements in the array *)
              ];

      typedef fiber = cont(unit);
      typedef fiber_fun = fun (unit / exh -> unit);
    (* function that spawns a fiber on a particular scheduler *)
      typedef spawn_fn = fun (fiber / exh -> );

    )

    type fiber = _prim (fiber)

    datatype bool = datatype bool

  (* signals for fibers *)
    datatype signal 
      = STOP
      | PREEMPT of fiber                (* the argument is the resumption fiber. *)
      | SLEEP of (fiber * long)         (* blocking for a given time interval. the second argument
					 * is the time to sleep in microseconds *)
      | UNBLOCK of (fiber * fiber)
      | BLOCK of fiber

    _primcode (
      typedef sched_act = cont(signal);
    )

  (* scheduler actions are continuations that consume a signal and perform a context switch *)
    type sched_act = _prim (sched_act)

    exception UnhandledSignal

  end
