(* spin-wait.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module provides a mechanism for busy waiting by spinning. Our protocol 
 * dictates that the time spent waiting starts with one CPU cycle and doubles at each
 * successive call. This process continues until maximum number of cycles, determined
 * by the quantity 2^maxSpinCyclesLg, has been reached, at which point we set the
 * time to one CPU cycle and continue.
 *)

structure SpinWait =
  struct

    _primcode (

      (* the function returned by this hlop is what does the actual waiting. this function
       * returns true when the sequence of calls has passed the maximum number of cycles. *)
	define inline @mk-spin-wait-fun (maxSpinCyclesLg : int) : fun( / -> bool) =
	    let spinCyclesLg : ![int] = alloc (1)
            let spinCyclesLg : ![int] = promote (spinCyclesLg)
	    fun doit () : bool =
		let spinCycles : int = I32LSh (1, #0(spinCyclesLg))
                fun spin (i : int) : () =
		    if I32Lt (i, spinCycles) then
			return ()
		    else
			apply spin (I32Add (i, 1))
		do apply spin (0)
		if I32Lte (#0(spinCyclesLg), maxSpinCyclesLg) then
		    do #0(spinCyclesLg) := I32Add (#0(spinCyclesLg), 1)
		    return (false)
		else
		    do #0(spinCyclesLg) := 1
		    return (true)
	    return (doit)
	  ;

    )

  end
