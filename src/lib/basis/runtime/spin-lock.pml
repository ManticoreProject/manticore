structure SpinLock =
  struct

    structure PT = PrimTypes

    _primcode (

      typedef sl_ty = SPIN_LOCK_TY;

      define @lock(lock : sl_ty / exh : PT.exh) : bool =
        fun spin () : bool =        
            if BCAS(&LOCK_OFFSET(lock), FALSE, TRUE)
	       then let mask : bool = vpload (ATOMIC, host_vproc)
	            do vpstore(ATOMIC, host_vproc, true)
                    return(mask)
	    else apply spin()
        apply spin()
      ;

      define @unlock(lock : sl_ty / exh : PT.exh) : () =
        do UPDATE(LOCK_OFFSET, lock, FALSE)
        do vpstore(ATOMIC, host_vproc, mask)
        return()
      ;
    )

  end
