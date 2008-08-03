structure SchedulerUtils =
  struct

    structure PT = PrimTypes

    _primcode (

    (* bootstrap the top level scheduler on each vproc. during execution, the runtime system
     * passes signals to our trampoline, which forwards those signals to the current scheduler. 
     *)
      define @bootstrap (vps : List.list / exh : PT.exh) : () =
        cont exit () = return()
      (* set the scheduler point just once *)
	let sk : cont(PT.signal) = vpload(VP_SCHED_CONT, host_vproc)
	do if NotEqual(sk, NIL)
	      then throw exit()
	   else return()
      (* pass signals to the current scheduler *)
	cont trampoline (k : PT.signal) =
	     Control.@forward(k / exh)
	let trampoline : cont(PT.signal) = promote(trampoline)
      (* store the trampoline in the vproc structure *)
	fun install (vps : List.list) : () =
	    case vps
	     of NIL => return()
	      | List.CONS (vp : vproc, vps : List.list) =>
		let sk : cont(PT.signal) = vpload(VP_SCHED_CONT, vp)
		if Equal(sk, NIL)
		   then do vpstore(VP_SCHED_CONT, vp, trampoline)
			apply install(vps)
		   else return()
	    end
	do apply install(vps)
        return()
      ;

    )

    val _ = Print.print "scheduler utils\n"

  end
