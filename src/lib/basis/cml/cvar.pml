(* cvar.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Condition variables, which are write-once unit-values synchronous memory
 * cells.
 *)

#include "debug.def"
#include "spin-lock.def"

structure CVar (*: sig

    type cvar

    val new : unit -> cvar
    val signal : cvar -> unit
    val waitEvt : cvar -> unit pevent
    val wait : cvar -> unit

  end*) = struct

    structure PT = PrimTypes
    structure PEvt = PrimEvent

    _primcode (
	typedef waiter = [
	    PrimEvent.event_state,	(* event-instance status flag *)
	    FLS.fls,			(* FLS of thread *)
	    vproc,			(* vproc affinity *)
	    PT.fiber			(* thread's continuation *)
	  ];
	typedef cvar = ![bool, bool, List.list];

      (* the fields of a cvar *)
#	define CV_LOCK		0
#	define CV_STATE		1
#	define CV_WAITING	2

      (* create a new signal variable *)
	define inline @cvar-new (_ : unit / _ : exh) : cvar =
	    let cv : cvar = alloc(false, false, nil)
	    let cv : cvar = promote (cv)
	    return (cv)
	  ;

      (* signal the variable, which wakes up any threads that are waiting on it. *)
	define @cvar-signal (cv : cvar / _ : exh) : unit =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    SPIN_LOCK(cv, CV_LOCK)
	      do UPDATE(CV_STATE, cv, true)
	      let waiting : list = SELECT(CV_WAITING, cv)
	      do UPDATE(CV_WAITING, cv, nil)
	    SPIN_UNLOCK(cv, CV_LOCK)
	    do SchedulerAction.@atomic-end (self)
	  (* loop over the list of waiting threads waking them *)
	    fun signalWaiting (l : list) : unit =
		  case l
		   of nil => return (UNIT)
		    | List.CONS(hd : waiter, tl : List.list) =>
			let flg : PEvt.event_state = #0(hd)
(* NOTE: this code doesn't work; probably because of a bug in the handling of BCAS in the
 * code generator.
			do if BCAS(&0(flg), PEvt.WAITING, PEvt.SYNCHED)
			    then (* enqueue waiting thread *)
			      let fls : FLS.fls = #1(hd)
			      let vp : vproc = #2(hd)
			      let k : PT.fiber = #3(hd)
			      (* in *)
				if Equal(self, vp)
				  then VProcQueue.@enqueue-in-atomic(vp, fls, k)
				  else VProcQueue.@enqueue-on-vproc(vp, fls, k)
			    else return()
*)
			let sts : PEvt.event_status = CAS(&0(flg), PEvt.WAITING, PEvt.SYNCHED)
			do if Equal(sts, PEvt.WAITING)
			    then  (* enqueue waiting thread *)
			      let fls : FLS.fls = #1(hd)
			      let vp : vproc = #2(hd)
			      let k : PT.fiber = #3(hd)
			      (* in *)
				if Equal(self, vp)
				  then VProcQueue.@enqueue-in-atomic(vp, fls, k)
				  else VProcQueue.@enqueue-on-vproc(vp, fls, k)
			    else return()
			apply signalWaiting (tl)
		  end
	    (* in *)
	    apply signalWaiting (waiting)
	  ;

      (* wait for a variable to be signaled *)
	define @cvar-wait (cv : cvar / _ : exh) : unit =
	    if SELECT(CV_STATE, cv)
	      then return (UNIT)
	      else (* slow-path requires waiting *)
		let self : vproc = SchedulerAction.@atomic-begin ()
		SPIN_LOCK(cv, CV_LOCK)
		if SELECT(CV_STATE, cv)
		  then
		    SPIN_UNLOCK(cv, CV_LOCK)
		    do SchedulerAction.@atomic-end (self)
		    return (UNIT)
		  else
		    cont k (_ : unit) = return (UNIT)
		    (* in *)
		      let flg : PEvt.event_state = alloc (PEvt.WAITING)
		      let fls : FLS.fls = FLS.@get()
		      let item : waiter = alloc (flg, fls, self, k)
		      let l : list = CONS(item, SELECT(CV_WAITING, cv))
		      let l : list = promote (l)
		      do UPDATE(CV_WAITING, cv, l)
		      SPIN_UNLOCK(cv, CV_LOCK)
		      SchedulerAction.@stop-from-atomic (self)
	  ;

	define inline @cvar-wait-evt (cv : cvar / _ : exh) : PEvt.pevent =
	    fun pollFn (_ : unit / _ : exh) : bool = return (SELECT(CV_STATE, cv))
	    fun doFn (k : PT.fiber / _ : exh) : unit = throw k (UNIT)
	    fun blockFn (flg : PEvt.event_state, fls : FLS.fls, k : cont(unit) / _ : exh) : PEvt.pevent =
		let self : vproc = SchedulerAction.@atomic-begin ()
		SPIN_LOCK(cv, CV_LOCK)
		if SELECT(CV_STATE, cv)
		  then
		    SPIN_UNLOCK(cv, CV_LOCK)
		    do SchedulerAction.@atomic-end (self)
		    return (UNIT)
		  else
		    let flg : PEvt.event_state = alloc (PEvt.WAITING)
		    let fls : FLS.fls = FLS.@get()
		    let item : waiter = alloc (flg, fls, self, k)
		    let l : list = CONS(item, SELECT(CV_WAITING, cv))
		    let l : list = promote (l)
		    do UPDATE(CV_WAITING, cv, l)
		    SPIN_UNLOCK(cv, CV_LOCK)
		    SchedulerAction.@stop-from-atomic (self)
	    (* in *)
	      return (PEvt.BEVT(pollFn, doFn, blockFn))
	  ;
      )

    type cvar = _prim (cvar)

    val new	: unit -> cvar = _prim(@cvar-new)
    val signal	: cvar -> unit = _prim(@cvar-signal)
    val waitEvt	: cvar -> unit PEvt.pevent = _prim(@cvar-wait-evt)
    val wait	: cvar -> unit = _prim(@cvar-wait)

  end
