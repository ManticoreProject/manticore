(* prim-synch.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive synchronization utilities.
 *)

structure PrimSynch =
  struct

    _primcode (

    (* spin wait until the condition isDone returns true *)
      define inline @spin-wait (isDone : fun (/ exh -> bool) / exh : exh) : () =
      (* catch any exception raised by isDone, and terminate the program *)
	cont exh' (e : exn) =
	  do assert(false)
	  throw exh(e)
	fun spin () : () =	
	      let done : bool = apply isDone(/ exh')
	      if done
		 then return()
	      else 
		  do Pause()      (* notify the hardware that the program is spinning *)
		  apply spin()
	apply spin ()
      ;

    )

  end
