(* vproc.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for vprocs.
 *)

#include "vproc-queue.def"

structure VProc =
  struct

    structure PT = PrimTypes

    _primcode (

      extern int GetNumVProcs ();
      extern void *SleepCont (void *) __attribute__((alloc));
      extern void *ListVProcs (void *) __attribute__((alloc));

      define @is-idle (vp : vproc / exh : exh) : bool =
	let isIdle : bool = vpload(VP_IDLE, vp)
	return(isIdle)
      ;

    (* get the unique id of a vproc *)
      define @id (vp : vproc / exh : exh) : int =
	let id : int = vpload(VPROC_ID, vp)
	return(id)
      ;

    (* number of vprocs *)
      define @num-vprocs (/ exh : exh) : int =
	let n : int = ccall GetNumVProcs()
	return(n)
      ;

    (* handle incoming messages *)
      define @handle-messages (/ exh : exh) : () =
        let m : PT.bool = vpload(ATOMIC, host_vproc)
        do vpstore(ATOMIC, host_vproc, PT.true)
        let messages : List.list = VProcQueue.@unload-and-get-messengers(/ exh)
        do Control.@run-fibers(messages / exh)
        do vpstore(ATOMIC, host_vproc, m)
	return()
      ;

    (* put the host vproc to sleep *)
      define @sleep (/ exh : exh) : () =
      (* resumption continuation for when the vproc awakens *)
	cont resumeK (x : PT.unit) = return ()
      (* the C runtime expects the resumption continuation to be in vp->stdCont *)
	do vpstore(STD_CONT, host_vproc, resumeK)
      (* fiber that puts the vproc to sleep *)
	let sleepK : PT.fiber = ccall SleepCont (host_vproc)
	throw sleepK(UNIT)
      ;

    (* wait for a thread to get enqueued the local vproc queue *)
      define @wait (/ exh : exh) : () =
        let m : bool = vpload (ATOMIC, host_vproc)
        do vpstore(ATOMIC, host_vproc, true)
        do @sleep(/ exh)
	do @handle-messages(/ exh)
        do vpstore(ATOMIC, host_vproc, m)
        return()
      ;

      define @all (/ exh : exh) : List.list =
	let vps : List.list = ccall ListVProcs(host_vproc)
        return(vps)
      ;

    (* returns all vprocs _except_ the host vproc *)
      define @others (/ exh : exh) : List.list =
        fun lp (vps : List.list, others : List.list / exh : exh) : List.list =
	    case vps
	     of nil => return(others)
	      | List.CONS(vp : vproc, vps : List.list) =>
		if Equal(vp, host_vproc)
                   then apply lp(vps, others / exh)
		else apply lp(vps, List.CONS(vp, others) / exh)
	    end
	let vps : List.list = ccall ListVProcs(host_vproc)
	apply lp(vps, nil / exh)  
      ;

    (* apply f to each vproc *)
      define @for-each(f : fun(vproc / exh ->) / exh : exh) : () =
	fun lp (vps : List.list / exh : exh) : () =
	    case vps
	     of nil => return()
	      | List.CONS(vp : vproc, vps : List.list) =>
		do apply f(vp / exh)
		apply lp(vps / exh)
	    end
	let vps : List.list = ccall ListVProcs(host_vproc)
	apply lp(vps / exh)
      ;

    (* apply f to each vproc except the host vproc *)
      define @for-others(f : fun(vproc / exh ->) / exh : exh) : () =
        let self : vproc = host_vproc
        fun g (vp : vproc / exh : exh) : () =
	    if NotEqual(vp, self)
	       then apply f(vp / exh)
	    else return()
        @for-each(g / exh)
      ;

    (* trigger a preemption on a remote vproc *)
      define @preempt-remote (vp : vproc / exh : exh) : () =
	do assert(NotEqual(vp, host_vproc))
	do vpstore(SIG_PENDING, vp, true)
	do vpstore(LIMIT_PTR, vp, 0)
	return()
      ;

    (* send a high-priority messenger fiber to the vproc *)
      define @send-messenger (vp : vproc, k : PT.fiber / exh : exh) : () =         
	do VProcQueue.@enqueue-on-vproc(vp, MESSENGER_FLS, k / exh)
        do @preempt-remote(vp / exh)
	return()
      ;

    )

  end
