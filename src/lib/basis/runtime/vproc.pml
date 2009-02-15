(* vproc.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Virtual processors.
 *)

structure VProc (* :
  sig

    _prim(

    (** support for local atomicity **)

      define inline @atomic-begin () : ();
      define inline @atomic-end () : ();

    (** unique ids **)

    (* unique id of a vproc *)
      define @vproc-id (vp : vproc / exh : exh) : int;
    (* find the vproc with a given unique id *)
      define @id-of-vproc (id : int / exh : exh) : vproc =
    (* total number of vprocs *)
      define @num-vprocs (/ exh : exh) : int;

    (** vproc allocation and iterators **)

    (* returns the list of all vprocs *)
      define @all-vprocs (/ exh : exh) : List.list;
    (* returns the list of all vprocs, but not the host vproc *)
      define @other-vprocs (/ exh : exh) : List.list;
    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : ();
    (* apply f to each vproc except the host vproc *)
      define @for-other-vprocs(f : fun(vproc / exh ->) / exh : exh) : ();

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (unit / exh -> unit), fls : FLS.fls, dst : vproc / exh : exh) : ();

    (** initialization and idling **)

    (* bootstrap the default scheduler *)
      define @boot-default-scheduler (act : PT.sched_act / exh : exh) : ();
    (* make the vproc go idle
     * NOTE: the C runtime is responsible for waking the vproc up when there is work to do.
     *)
      define @wait (/ exh : exh) : ();

    )
    
  end *) = struct

    structure PT = PrimTypes

    _primcode (

    (* hooks into the C runtime system (parallel-rt/vproc/vproc.c) *)
      extern void* GetNthVProc(int);
      extern int GetNumVProcs ();
      extern void *SleepCont (void *) __attribute__((alloc));
      extern void *ListVProcs (void *) __attribute__((alloc));

    (* support for local atomicity *)
      define inline @atomic-begin () : () =
	  do vpstore (ATOMIC, host_vproc, true)
	    return ()
      ;

      define inline @atomic-end () : () =
	  let vp : vproc = host_vproc
	  do vpstore (ATOMIC, vp, false)
	  let pending : bool = vpload (SIG_PENDING, vp)
	    if pending
	      then
		do vpstore (SIG_PENDING, vp, false)
                cont exh (x : exn) = return(UNIT)
		SchedulerAction.@yield(/ exh)
	      else return (UNIT)
      ;

    (* total number of vprocs *)
      define @num-vprocs (/ exh : exh) : int =
	let n : int = ccall GetNumVProcs()
	return(n)
     ;

    (* returns the unique id of the given vproc *)
      define @vproc-id (vp : vproc / exh : exh) : int =
	let id : int = vpload(VPROC_ID, vp)
	return(id)
      ;

    (* find the vproc with a given unique id *)
      define @id-of-vproc (id : int / exh : exh) : vproc =
        let max : int = @num-vprocs(/ exh)
        do assert(I32Lt(id, max))
        do assert(I32Gte(id, 0))
	let vp : vproc = ccall GetNthVProc(id)
        return(vp)
      ;

    (** vproc allocation and iterators  **)

    (* returns the list of all vprocs *)
      define @all-vprocs (/ exh : exh) : List.list =
	let vps : List.list = ccall ListVProcs(host_vproc)
        return(vps)
      ;

    (* returns the list of all vprocs, but not the host vproc *)
      define @other-vprocs (/ exh : exh) : List.list =
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
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : () =
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
      define @for-other-vprocs(f : fun(vproc / exh ->) / exh : exh) : () =
        let self : vproc = host_vproc
        fun g (vp : vproc / exh : exh) : () =
	    if NotEqual(vp, self)
	       then apply f(vp / exh)
	    else return()
        @for-each-vproc(g / exh)
      ;

    (* spawn a thread on a remote vproc *)
      define @spawn-on (f : fun (unit / exh -> unit), fls : FLS.fls, dst : vproc / exh : exh) : () =
	cont fiber (x : unit) =
	  cont exh (exn : exn) = 
	    let _ : unit = SchedulerAction.@stop(/ exh)
            return()
	  let (_ : unit) = apply f (UNIT / exh)
	  let _ : unit = SchedulerAction.@stop(/ exh)
          return()
	do VProcQueue.@enqueue-on-vproc (dst, fls, fiber / exh)
	return ()
      ;


    (** initialization and idling **)

    (* the trampoline is a continuation that receives asynchronous signals generated by the C runtime, and
     * passes those signals to the scheduler action at the top of the scheduler-action stack.
     *
     * IMPORTANT: this operation must precede any other scheduling operations, and signals must be masked before
     * this operation completes.
     *)
      define @set-trampoline ( / exh : exh) : () =
        do vpstore(ATOMIC, host_vproc, true)

      (* the trampoline passes signals from the C runtime to the current scheduler. there are two possibilities:
       *  1. the vproc was awoken from an idle state
       *  2. a timer interrupt arrived
       *)
	cont trampoline (k : PT.fiber) = 
	  if Equal(k, M_NIL)
             then 
	      (* case 1 *)
	      SchedulerAction.@forward(PT.STOP / exh)
	  else 
	      (* case 2 *)
	      SchedulerAction.@forward(PT.PREEMPT(k) / exh)
	let trampoline : cont(PT.fiber) = promote(trampoline)

      (* set the trampoline on a given vproc *)
	fun setTrampoline (vp : vproc / exh : exh) : () =
	    let currentTrampoline : cont(PT.fiber) = vpload(VP_SCHED_CONT, vp)
	    do assert(Equal(currentTrampoline, nil))
	    do vpstore(VP_SCHED_CONT, vp, trampoline)
	    return()
	do @for-each-vproc(setTrampoline / exh)

        return()
      ;

    (* push a scheduler action on a remote vproc's stack.
     * NOTE: because this operation is not "thread safe", we should only use it during runtime 
     * initialization.
     *)
      define @push-remote-act (vp : vproc, act : PT.sched_act / exh : exh) : () =
	do assert(NotEqual(act, nil))
	let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
	let item : [PT.sched_act, any] = alloc (act, (any)stk)
	let item : [PT.sched_act, any] = promote (item)
	do vpstore (VP_ACTION_STK, vp, item)
        return()
      ;

    (* push a copy of the top-level scheduler on each vproc (except the host)  *)
      define @seed-remote-action-stacks (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
	fun f (vp : vproc / exh : exh) : () =
	    let act : PT.sched_act = apply mkAct (vp / exh)
            @push-remote-act(vp, act / exh)
	@for-other-vprocs(f / exh)
      ;

    (* initialize scheduling code on each vproc (except the host). *)
      define @initialize-remote-schedulers (fls : FLS.fls / exh : exh) : () =
	let self : vproc = host_vproc
        do vpstore(ATOMIC, self, true)
	cont wakeupK (x : PT.unit) = 
	     let _ : PT.unit = SchedulerAction.@stop(/ exh)
	     return()
	fun f (vp : vproc / exh : exh) : () = VProcQueue.@enqueue-on-vproc(vp, fls, wakeupK / exh)
	do @for-other-vprocs(f / exh)
        do vpstore(ATOMIC, self, false)
        return()
      ;

    (* bootstrap the default scheduler *)
      define @boot-default-scheduler (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
        do vpstore(ATOMIC, host_vproc, true)
        let fls : FLS.fls = FLS.@get(/ exh)
	do @set-trampoline (/ exh)
        do @seed-remote-action-stacks(mkAct / exh)
	cont startLeadK (_ : PT.unit) = @initialize-remote-schedulers(fls / exh)
	let act : PT.sched_act = apply mkAct (host_vproc / exh)
	SchedulerAction.@run(act, startLeadK / exh)
      ;

    (* put the host vproc to sleep.
     * NOTE: the C runtime is responsible for waking the vproc up when there is work to do
     * (see SleepCont in parallel-rt/vproc.c).
     *)
      define @wait (/ exh : exh) : () =
        fun sleep () : () =
	    cont wakeupK (x : unit) = return ()
	  (* the C runtime expects the resumption continuation to be in vp->wakeupCont *)
	    do vpstore(VP_WAKEUP_CONT, host_vproc, wakeupK)
	  (* sleepK puts the vproc to sleep *)
	    let sleepK : PT.fiber = ccall SleepCont (host_vproc)
	    throw sleepK(UNIT)

        let m : PT.bool = vpload (ATOMIC, host_vproc)
        do vpstore(ATOMIC, host_vproc, true)
        do apply sleep()
        do vpstore(ATOMIC, host_vproc, m)

        return()
      ;

    (* mask signals before running any scheduling code *)
      define @mask-signals(x : unit / exh : exh) : unit =
	do vpstore(ATOMIC, host_vproc, true)
	return(UNIT)
      ;

    (* initialize fls *)
      define @init-fls (x : unit / exh : exh) : unit = 
	let fls : FLS.fls = FLS.@new (UNIT / exh)
	FLS.@set(fls / exh)
      ;

    )

  (* signals must be masked before initializing the rest of the runtime *)
    val maskSignals : unit -> unit = _prim(@mask-signals)
    val () = maskSignals()

  (* create fls for the root thread *)
    val initFLS : unit -> unit = _prim(@init-fls)
    val () = initFLS() 


  end
