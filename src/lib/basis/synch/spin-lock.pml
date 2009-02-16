(* spin-lock.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * FIXME: this code should be moved to "include/spin-lock.def" file and
 * restructured as a set of macros:
 *
 *	#define SPIN_LOCK(name, ty, OFFSET)
 *	#define SPIN_UNLOCK(name, ty, OFFSET)
 *
 * Also: remove the masking/unmasking of signals.
 *)

structure SPIN_LOCK_NAME =
  struct

    _primcode (

      typedef sl_ty = SPIN_LOCK_TY;

      define @lock (lock : sl_ty / exh : exh) : bool =
        fun spin () : bool =
	    (* try to acquire the lock *)
	    if TAS(ADDR_OF(LOCK_OFFSET, lock))
	       then 
		(*do Pause()*) (* allow other hardware threads to run *)
		apply spin()		
	    else 
		let mask : bool = vpload (ATOMIC, host_vproc)
	        return(mask)
	do vpstore(ATOMIC, host_vproc, true)
        apply spin()
      ;

      define @unlock (lock : sl_ty, mask : bool / exh : exh) : () =
        do UPDATE(LOCK_OFFSET, lock, false)
        do vpstore(ATOMIC, host_vproc, mask)
        return()
      ;
    )

  end
