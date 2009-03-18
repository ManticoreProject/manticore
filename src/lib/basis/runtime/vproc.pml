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

    (** Unique ids **)

    (* unique id of a vproc *)
      define @vproc-id (vp : vproc) : int;
    (* find the vproc with a given unique id *)
      define @vproc-by-id (id : int) : vproc =
    (* total number of vprocs *)
      define @num-vprocs (/ exh : exh) : int;

    (** Vproc lists and iterators **)

    (* returns the list of all vprocs *)
      define @all-vprocs () : List.list;
    (* returns the list of all vprocs, excluding host vproc. 
     * NOTE: signals must be masked before the call.
     *)
      define @other-vprocs-from-atomic (self : vproc / exh : exh) : List.list;
    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : ();
    (* apply f to each vproc except the host vproc. *)
      define @for-other-vprocs-from-atomic (self : vproc, f : fun(vproc / exh ->) / exh : exh) : ();

    (** Signaling and sleeping **)

    (* place a signal on the landing pad of the remote vproc.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @send-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : ();
    (* place a high-priority signal on the landing pad of the remote vproc. the vproc is guaranteed
     * to handle the signal within a constant number of computational steps.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @interrupt-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : ();
    (* trigger a preemption on a remote vproc.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @preempt-from-atomic (self : vproc, dst : vproc) : ();
    (* receive pending signals from the host vproc's landing pad.
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @recv-from-atomic (self : vproc) : queue_item;
    (* put the vproc to sleep until a signal arrives on its landing pad 
     * PRECONDITION: Equal(self, host_vproc)
     *)
      define @sleep-from-atomic (vp : vproc) : ();

    (** Initialization **)

    (* bootstrap the vproc.
     *   - mkAct is a function that takes a vproc and returns the top-level scheduler
     *     for that vproc.
     *)
      define @bootstrap (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : ();

    )
    
  end *) = struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode (

    (* hooks into the C runtime system (parallel-rt/vproc/vproc.c) *)
      extern void* GetNthVProc (int);
      extern int GetNumVProcs ();
      extern void *SleepCont (void *) __attribute__((alloc));
      extern void *ListVProcs (void *) __attribute__((alloc));
      extern void VProcWake (void *);
      extern void VProcPreempt (void *);

    (* returns the total number of vprocs *)
      define inline @num-vprocs () : int =
	  let n : int = ccall GetNumVProcs()
	  return (n)
	;

    (* returns the unique id of the given vproc *)
      define inline @vproc-id (vp : vproc) : int =
	  let id : int = vpload(VPROC_ID, vp)
	  return (id)
	;

    (* find the vproc with a given unique id *)
      define @vproc-by-id (id : int) : vproc =
#ifndef NDEBUG
	  let max : int = @num-vprocs()
	  do assert(I32Lt(id, max))
	  do assert(I32Gte(id, 0))
#endif
	  let vp : vproc = ccall GetNthVProc(id)
	  return (vp)
	;

    (** vproc allocation and iterators  **)

    (* returns the list of all vprocs *)
      define @all-vprocs () : List.list =
	  let vps : List.list = ccall ListVProcs(host_vproc)
	  return(vps)
	;

    (* returns the list of all vprocs, excluding the host vproc. 
     * NOTE: signals must be masked before the call.
     *)
      define @other-vprocs-from-atomic (self : vproc / exh : exh) : List.list =
	  fun lp (vps : List.list, others : List.list) : List.list =
	      case vps
	       of nil => return(others)
		| List.CONS(vp : [vproc], vps : List.list) =>
		    if Equal(#0(vp), self)
		      then apply lp(vps, others)
		      else apply lp(vps, List.CONS(vp, others))
	      end
	  let vps : List.list = ccall ListVProcs(self)
	  apply lp(vps, nil)
	;

    (* apply f to each vproc *)
      define @for-each-vproc(f : fun(vproc / exh ->) / exh : exh) : () =
	  fun lp (vps : List.list) : () =
	      case vps
	       of nil => return()
		| List.CONS(vp : [vproc], vps : List.list) =>
		  do apply f(#0(vp) / exh)
		  apply lp(vps)
	      end
	  let vps : List.list = ccall ListVProcs(host_vproc)
	  apply lp(vps)
	;

    (* apply f to each vproc except the host vproc. 
     * NOTE: the functions are applied in
     *)
      define @for-other-vprocs-from-atomic (self : vproc, f : fun(vproc / exh ->) / exh : exh) : () =
	  fun g (vp : vproc / exh : exh) : () =
		if NotEqual(vp, self) then apply f(vp / exh)
		else return()
	  @for-each-vproc(g / exh)
	;

    (** Signaling and sleeping **)

    (* place a signal on the landing pad of the remote vproc.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @send-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : () =
          do assert(NotEqual(self, dst))
          cont exit () = return()
	  fun lp () : () =
	      let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, dst)
	      let ldgPadNew : queue_item = alloc(fls, k, ldgPadOrig)
	      let ldgPadNew : queue_item = promote(ldgPadNew)
	      let x : queue_item = CAS(&VP_LANDING_PAD(dst), ldgPadOrig, ldgPadNew)
	      if NotEqual(x, ldgPadOrig)
		then
		  do Pause ()
		  apply lp ()
		else
		    let sleeping : bool = vpload(VP_SLEEPING, dst)
		    do if sleeping
		          then 
			   do ccall VProcWake(dst)
		           return()
		       else 
			   return()
                    throw exit()
	  do apply lp()
	  return()
      ;

    (* place a high-priority signal on the landing pad of the remote vproc. the vproc is guaranteed
     * to handle the signal within a constant number of computational steps.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @interrupt-from-atomic (self : vproc, dst : vproc, fls : FLS.fls, k : PT.fiber) : () =
          do @send-from-atomic(self, dst, fls, k)          
          do ccall VProcPreempt(self, dst)
	  return()
      ;

    (* trigger a preemption on a remote vproc.
     * PRECONDITION: NotEqual(self, dst) and Equal(self, host_vproc)
     *) 
      define @preempt-from-atomic (self : vproc, dst : vproc) : () =
          cont k (x : unit) =
	    let _ : unit = SchedulerAction.@stop()
            return()
          let fls : FLS.fls = FLS.@get()
          do @send-from-atomic(self, dst, fls, k)
          do ccall VProcPreempt(self, dst)
	  return()
      ;

    (* receive pending signals from the host vproc's landing pad.
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @recv-from-atomic (self : vproc) : queue_item =
          cont exit () = return(Q_EMPTY)
          let ldgPadOrig : queue_item = vpload(VP_LANDING_PAD, self)
          do if Equal(ldgPadOrig, Q_EMPTY)
		then throw exit()
	     else return()
          let x : queue_item = CAS(&VP_LANDING_PAD(self), ldgPadOrig, Q_EMPTY)
          if Equal(x, ldgPadOrig)
	     then return(x)
	  else return(Q_EMPTY)
      ;

    (* put the vproc to sleep until a signal arrives on its landing pad 
     * PRECONDITION: Equal(vp, host_vproc)
     *)
      define @sleep-from-atomic (vp : vproc) : () =
	  fun sleep () : () =	      
              cont wakeupK (x : unit) = return ()
	    (* the C runtime expects the resumption continuation to be in vp->wakeupCont *)
	      do vpstore(VP_WAKEUP_CONT, vp, wakeupK)
	      let sleepK : PT.fiber = ccall SleepCont (vp)
	      throw sleepK(UNIT)
	  do apply sleep()
	  return()
	;

    (** Initialization **)

    (* the schedCont is a continuation that receives asynchronous signals generated by the C runtime, and
     * passes those signals to scheduling code in Manticore.
     *
     * IMPORTANT: this operation must precede any other scheduling operations, and signals must be masked before
     * this operation completes.
     *)
      define @init-sched-cont ( / exh : exh) : () =

	(* the fiber k is passed to schedCont from the C runtime. there are two cases that the schedCont
	 * must handle:
	 *   case 1: the vproc was awoken from an idle state.
	 *   case 2: an interrupt has arrived.
	 *)
	  cont schedCont (k : PT.fiber) = 
	    if Equal(k, M_NIL)
	      then  (* case 1 *)
		SchedulerAction.@forward(PT.STOP)
	      else (* case 2 *)
		SchedulerAction.@forward(PT.PREEMPT(k))

        (* set the schedCont field in each vproc *)
	  let schedCont : cont(PT.fiber) = promote(schedCont)
	  fun set (vp : vproc / exh : exh) : () =
	      let currentSchedCont : cont(PT.fiber) = vpload(VP_SCHED_CONT, vp)
	      do assert(Equal(currentSchedCont, nil))
	      do vpstore(VP_SCHED_CONT, vp, schedCont)
	      return()
	  do @for-each-vproc(set / exh)

          return()
	;

    (* the dummy fiber is a trivial fiber that terminates immediately. the C runtime uses this fiber for 
     * various tasks.
     *)
      define @init-dummy-k (/ exh : exh) : () =

	  cont dummyK (x : unit) = 
	    let _ : unit = SchedulerAction.@stop()
	    return()

        (* set the dummy fiber field in each vproc *)
	  let dummyK : PT.fiber = promote(dummyK)
	  fun set (vp : vproc / exh : exh) : () =
	      do vpstore(VP_DUMMYK, vp, dummyK)
	      return()
	  do @for-each-vproc(set / exh)

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
      define @seed-remote-action-stacks-from-atomic (self : vproc, mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
	  fun f (vp : vproc / exh : exh) : () =
	      let act : PT.sched_act = apply mkAct (vp / exh)
	      @push-remote-act(vp, act / exh)
	  @for-other-vprocs-from-atomic(self, f / exh)
	;

    (* bootstrap the vproc.
     *   - mkAct is a function that takes a vproc and returns the top-level scheduler
     *     for that vproc.
     *)
      define @bootstrap (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
	  let self : vproc = SchedulerAction.@atomic-begin()
	  let fls : FLS.fls = FLS.@get()
	  do @init-sched-cont(/ exh)
	  do @init-dummy-k(/ exh)
	  do @seed-remote-action-stacks-from-atomic(self, mkAct / exh)
	  cont startLeadK (_ : PT.unit) = return()
	  let act : PT.sched_act = apply mkAct (self / exh)
	  SchedulerAction.@run(self, act, startLeadK)
	;

    (* mask signals before running any scheduling code *)
      define @mask-signals (x : unit / exh : exh) : unit =
	  let vp : vproc = SchedulerAction.@atomic-begin()
	  return (UNIT)
	;

    (* initialize fls *)
      define @init-fls (x : unit / exh : exh) : unit = 
	  let fls : FLS.fls = FLS.@new (UNIT / exh)
	  do FLS.@set(fls)
	  return (UNIT)
	;

    )

  (* signals must be masked before initializing the rest of the runtime *)
    val maskSignals : unit -> unit = _prim(@mask-signals)
    val () = maskSignals()

  (* create fls for the root thread *)
    val initFLS : unit -> unit = _prim(@init-fls)
    val () = initFLS() 

  end
