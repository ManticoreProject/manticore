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
	      if I32Eq(#0(lock), 0) then
	      (* try to acquire the lock *)
		if TAS(&LOCK_OFFSET(lock)) then
		  let mask : PT.bool = vpload (ATOMIC, host_vproc)
		    return(mask)
		else apply spin()
	      else (* spin *)
		do Pause() (* allow other hardware threads to run *)
		  apply spin()
	do vpstore(ATOMIC, host_vproc, PT.true)
        apply spin()
      ;

      define @unlock(lock : sl_ty, mask : PT.bool / exh : PT.exh) : () =
        do UPDATE(LOCK_OFFSET, lock, (PT.bool)PT.false)
        do vpstore(ATOMIC, host_vproc, mask)
        return()
      ;
    )

  end
