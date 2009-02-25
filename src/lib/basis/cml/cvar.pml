(* cvar.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Condition variables, which are write-once unit-values synchronous memory
 * cells.
 *)

structure CVar (*: sig

    type cvar

    val new : unit -> cvar
    val signal : cvar -> unit
    val waitEvt : cvar -> unit pevent
    val wait : cvar -> unit

  end*) = struct

    _primcode (
	typedef waiter = [PrimEvent.dirty_flag, FLS.fls, vproc, PT.fiber];
	typedef cvar = ![bool, bool, List.list];

      (* the fields of a cvar *)
#	define CV_LOCK		0
#	define CV_STATE		1
#	define CV_WAITING	2

	define inline @cvar-new (_ : unit / _ : exh) : cvar =
	    let cv : cvar = alloc(false, false, nil)
	    let cv : cvar = promote (cv)
	    return (cv)
	  ;

      (* lock a condition variable *)
	define inline @cvar-lock (cv : cvar / ) : () =
	    fun spinLp () : () = if ADDR_OF(CV_LOCK, cv) then
		    do Pause()
		    apply spinLp ()
		  else if TAS(ADDR_OF(CV_LOCK, cv))
		    then apply spinLp ()
		    else return ()
	    apply spinLp ()
	  ;

      (* create a new signal variable *)
	define inline @cvar-new (_ : unit / _ : exh) : cvar =
	    let sv : cvar = alloc (false, false, List.nil)
	    return (sv)
	  ;

      (* signal the variable, which wakes up any threads that are waiting on it. *)
	define @cvar-signal (cv : cvar / _ : exh) : unit =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    do @cvar-lock (cv)
	    do UPDATE(CV_STATE, cv, true)
	    let waiting : list = SELECT(CV_WAITING, cv)
	    do UPDATE(CV_WAITING, cv, nil)
	    do @cvar-unlock (cv)
	    do SchedulerAction.@atomic-end (self)
	  (* loop over the list of waiting threads waking them *)
	    fun signalWaiting (l : list) : unit =
		  case l
		   of nil => return (UNIT)
		    | List.CONS(hd : waiter, tl : List.list) =>
			let flg : dirty_flag = #0(hd)
			do if BCAS(&0(flg), WAITING, SYNCHED)
			    then (* enqueue waiting thread *)
			      let fls : FLS.fls = #1(hd)
			      let vp : vproc = #2(hd)
			      let k : fiber = #3(hd)
			      (* in *)
				if Equal(self, vp)
				  then VProcQueue.@enqueue-in-atomic(fls, k)
				  else VProcQueue.@enqueue-on-vproc(vp, fls, k)
			    else return()
			apply signalWaiting (tl)
		  end
	    (* in *)
	    apply signalWaiting ()
	;

      (* wait for a variable to be signaled *)
	define @cvar-wait (cv : cvar / _ : exh) : unit =
	    if SELECT(CV_STATE, cv)
	      then return (UNIT)
	      else (* slow-path requires waiting *)
		let self : vproc = SchedulerAction.@atomic-begin ()
		do @cvar-lock (cv)
		if SELECT(CV_STATE, cv)
		  then
		    do SchedulerAction.@atomic-end (self)
		    return (UNIT)
		  else
		    cont k (_ : unit) = return (UNIT)
		    (* in *)
		      let flg : dirty_flag = alloc (WAITING)
		      let fls : FLS.fls = FLS.@get()
		      let item : waiter = alloc (flg, fls, self, k)
		      let l : list = CONS(item, SELECT(CV_WAITING, cv))
		      let l : waiter = promote (cons)
		      do UPDATE(CV_WAITING, cv, l)
		      SchedulerAction.@stop-from-atomic (self)
	  ;

	define inline @cvar-wait-evt (cv : cvar / _ : exh) : pevent =
	    fun pollFn (_ : unit / _ : exh) : bool = return (SELECT(CV_STATE, cv))
	    fun doFn (k : PT.fiber / _ : exh) : unit = throw k (UNIT)
	    fun blockFn (flg : PrimEvent.dirty_flag, fls : FLS.fls, k : cont(unit) / _ : exh) : PrimEvent.pevent =
		let self : vproc = SchedulerAction.@atomic-begin ()
		do @cvar-lock (cv)
		if SELECT(CV_STATE, cv)
		  then
		    do SchedulerAction.@atomic-end (self)
		    return (UNIT)
		  else
		    let flg : dirty_flag = alloc (WAITING)
		    let fls : FLS.fls = FLS.@get()
		    let item : waiter = alloc (flg, fls, self, k)
		    let l : list = CONS(item, SELECT(CV_WAITING, cv))
		    let l : waiter = promote (cons)
		    do UPDATE(CV_WAITING, cv, l)
		    SchedulerAction.@stop-from-atomic (self)
	  ;
      )

    type cvar = _prim (cvar)

    val new	: unit -> cvar = _prim(@cvar-new)
    val signal	: cvar -> unit = _prim(@cvar-signal)
    val waitEvt	: cvar -> unit pevent = _prim(@cvar-wait-evt)
    val wait	: cvar -> unit = _prim(@cvar-wait)

  end
