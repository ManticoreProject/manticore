(* init.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * IMPORTANT: must load this file first to correctly initialize the vproc structure
 *)

structure Init =
  struct

     _primcode (
      define @init-vproc(x : PrimTypes.unit / exh : PrimTypes.exh) : PrimTypes.unit =
      (* mask signals *)
	do vpstore(ATOMIC, host_vproc, TRUE)
	return(UNIT)
      ;
    )

    val initVProc : unit -> unit = _prim(@init-vproc)
    val _ = initVProc()

  end
