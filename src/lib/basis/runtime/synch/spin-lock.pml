(* spin-lock.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure SPIN_LOCK_NAME =
  struct

    structure PT = PrimTypes

    _primcode (

      typedef sl_ty = SPIN_LOCK_TY;

      define @lock(lock : sl_ty / exh : PT.exh) : PT.bool =
        fun spin () : PT.bool =        
            if BCAS(&LOCK_OFFSET(lock), FALSE, TRUE)
	       then let mask : PT.bool = vpload (ATOMIC, host_vproc)
	            do vpstore(ATOMIC, host_vproc, TRUE)
                    return(mask)
	    else apply spin()
        apply spin()
      ;

      define @unlock(lock : sl_ty, mask : PT.bool / exh : PT.exh) : () =
        do UPDATE(LOCK_OFFSET, lock, FALSE)
        do vpstore(ATOMIC, host_vproc, mask)
        return()
      ;
    )

  end
