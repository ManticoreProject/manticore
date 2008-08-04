(* scheduler-utils.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Basic utilities for building schedulers:
 *  - bootstrapping for the top-level scheduler
 *  - thread spawning, switching, and dispatching operations
 *  - basic scheduler initialization
 *  - round-robin scheduler
 *)

structure SchedulerUtils =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

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

    (* switch to a given thread *)
      define @switch-to (fls : FLS.fls, fiber : PT.fiber / exh : PT.exh) noreturn =
	let _ : PT.unit =  FLS.@set (fls / exh)
	do vpstore (ATOMIC, host_vproc, FALSE)
	throw fiber (UNIT)
      ;

    (* switch to the next thread in the host vproc's ready queue *)
      define @dispatch (/ exh : PT.exh) noreturn =
	  let qitem : VProcQueue.queue = VProcQueue.@dequeue ( / exh)
	  let item : [FLS.fls, PT.fiber, VProcQueue.queue] = ([FLS.fls, PT.fiber, VProcQueue.queue]) qitem
	  @switch-to (#0(item), #1(item) / exh)
      ;

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (PT.unit / PT.exh -> PT.unit), fls : FLS.fls, dst : vproc / exh : PT.exh) : () =
	cont fiber (x : PT.unit) =
	  cont exh (exn : exn) = @dispatch ( / exh)
	  let (_ : PT.unit) = apply f (UNIT / exh)
	  @dispatch ( / exh)
	do VProcQueue.@enqueue-on-vproc (dst, fls, fiber / exh)
	return ()
      ;

    (* initialize a given scheduler on  a collection of vprocs *)
      define @scheduler-startup (mkAct : fun (vproc / PT.exh -> PT.sigact), fls : FLS.fls, vps : List.list / exh : PT.exh) : () =
	(* bootstrap on the vprocs *)
	  do @bootstrap (vps / exh)

	  let syncPoint : ![int] = alloc(0)
	 (* since vprocs share it, promote syncPoint *)
	  let syncPoint : ![int] = promote(syncPoint)

	  let self : vproc = host_vproc

	  let length : fun(List.list / PT.exh -> Int.ml_int) = pmlvar List.length
	  let nVProcs : Int.ml_int = apply length (vps / exh)
	 (* only count other vprocs *)
	  let nVProcs : int = I32Sub (#0(nVProcs), 1)

	 (* wait for all vprocs to have started the init function *)
	 fun spinWait (_ : PT.unit / exh : PT.exh) : () = 
	      let i : int = #0(syncPoint)     
	      if I32Eq (i, nVProcs) then	  
		return ()
	      else
		apply spinWait (UNIT / exh)

	 (* activate the scheduler *)
	  fun init (_ : PT.unit / exh : PT.exh) : PT.unit =	  
		do vpstore (ATOMIC, host_vproc, TRUE)
	       (* dummy fiber synchronizes on the barrier and then exits immediately to activate the scheduler *)
		cont dummyK (_ : PT.unit) = 
		     do vpstore (ATOMIC, host_vproc, TRUE)
      (*               print_debug("initializing scheduler")*)
		     let x : int = I32FetchAndAdd (syncPoint, 1)
		     do apply spinWait (UNIT / exh)
		     do Control.@forward (STOP / exh)
		     return(UNIT)
	      (* make the scheduler instance for the host vproc *)
	       let act : PT.sigact = apply mkAct (host_vproc / exh)
	       do Control.@run-thread (act, dummyK, fls / exh)
	       return(UNIT)

	 (* spawn the init function on the vprocs other than the initial vproc *)
	  fun spawnOnAll (vps : List.list / exh : PT.exh) : () =
		case vps
		 of NIL => return ()
		  | List.CONS (vp:vproc, rest:List.list) =>
		      if Equal(vp, self) then
                       (* do not spawn on the initial vproc *)
			apply spawnOnAll (rest / exh)
		      else
			do @spawn-on (init, fls, vp / exh)
			  apply spawnOnAll (rest / exh)
		end

	  cont startup (_ : PT.unit) =
		do vpstore(ATOMIC, self, TRUE)
	      (* install the scheduler on remote vprocs *)
		do apply spawnOnAll (vps / exh)
		apply spinWait (UNIT / exh)

	(* make the scheduler instance for the host vproc *)
	 let act : PT.sigact = apply mkAct (host_vproc / exh)
	 do Control.@run-thread (act, startup, fls / exh)
	 return()
      ;

      extern void *ListVProcs (void *) __attribute__((alloc));

    (* top-level thread scheduler that uses a round robin policy *)
      define @round-robin ( / exh : PT.exh) : () = 
	cont switch (s : PT.signal) =
	  let vp : vproc = host_vproc
	  let atomic : PT.bool = vpload(ATOMIC, vp)
	  do assert(atomic)

	  cont dispatch () =
	    let qitem : VProcQueue.queue = VProcQueue.@dequeue ( / exh)
	    let item : [FLS.fls, PT.fiber, VProcQueue.queue] = ([FLS.fls, PT.fiber, VProcQueue.queue]) qitem
	    let fls : FLS.fls = #0 (item)
	    let fiber : PT.fiber = #1 (item)
	    Control.@run-thread (switch, fiber, fls / exh)

	  case s
	    of STOP => throw dispatch ()
	     | PT.PREEMPT (k : PT.fiber) =>
		 let fls : FLS.fls = FLS.@get ( / exh)
		 do VProcQueue.@enqueue (fls, k / exh)
      (*print_msg("preempt")*)
		 throw dispatch () 
	  end

	fun mkSwitch (_ : vproc / exh : PT.exh) : PT.sigact = return (switch)

       (* get handles for all vprocs *)
	let vps : List.list = ccall ListVProcs(host_vproc)
       (* fiber-local storage for the top-level scheduler *)
	let fls : FLS.fls = FLS.@new (UNIT / exh)
       (* run the scheduler on all vprocs *)
	do @scheduler-startup (mkSwitch, fls, vps / exh)
	return ()
      ;

    )

    val _ = Print.print "scheduler utils\n"

  end
