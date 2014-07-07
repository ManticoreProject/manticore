(* no-op-scheduler.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure NoOpScheduler =
  struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode(
      define @no-op-scheduler (_ : unit / exh : exh) : unit = 
	do VProcInit.@bootstrap-sequential ( / exh)
	return (UNIT)
      ;
    )

    val noOpScheduler : unit -> unit = _prim (@no-op-scheduler)
    val _ = noOpScheduler()

  end
