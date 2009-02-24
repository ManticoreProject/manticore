(* prim-event.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimEvent (*: sig

  (* primitive events *)
    type 'a pevent
 
    val never : 'a pevent
    val always : 'a -> 'a pevent
    val choose : 'a pevent * 'a pevent -> 'a pevent
    val wrap : ('a pevent * ('a -> 'b)) -> 'b pevent
    val sync : 'a pevent -> 'a

  (* signal variables *)
    type signal_var

    val signalVar : unit -> signal_var
    val signal : signal_var -> unit
    val waitEvt : signal_var -> unit pevent
    val wait : signal_var -> unit

  end*) = struct

    type 'a cont = _prim (cont(any))

    datatype event_status = WAITING | CLAIMED | SYNCHED

    type dirty_flag = _prim(![event_status])

    datatype 'a pevent
      = CHOOSE of ('a pevent * 'a pevent)
      | BEVT of (unit -> bool) * ('a cont -> unit) * ((flag * FLS.fls * 'a cont) -> unit)

  (* optimistically poll the base events *)
    fun poll (evt, enabled) = (case evt
	   of CHOOSE(evt1, evt2) => poll (evt2, poll (evt1, enabled))
	    | BEVT(pollFn, doFn, blockFn) => if pollFn()
		then doFn::enabled
		else enabled
	  (* end case *))

    _primcode (
	define inline @base-event (
	    pollFn  : fun(unit / exh -> bool),
	    doFn    : fun(cont(any) / exh -> unit),
	    blockFn : fun(dirty_flag, cont(any) / exn -> unit)
	  ) : pevent =
	      return BEVT(pollFn, doFn, blockFn)
	;

	define inline @always (x : any / _ : exh) : evt =
	    fun pollFn (_ : unit / _ : exh) : bool = return (true)
	    fun doFn (k : cont(any) / _ : exh) : unit = throw k(x)
	    fun blockFn (_ : dirty_flag, _ : cont(any) / exh : exh) : unit =
		  throw exh (UNIT)
	    (* in *)
	      return BEVT(pollFn, doFn, blockFn)
	;

	define @wrap (ev : evt, f : fun(any / exh -> any) / exh : exh) : evt =
	    fun wrapf (ev : evt / exh : exh) : evt =
		  case ev
		   of CHOOSE(ev1, ev2) =>
			let ev1' : evt = apply wrapf (ev / exh)
			let ev2' : evt = apply wrapf (ev / exh)
			(* in *)
			  return (CHOOSE(ev1', ev2'))
		    | BEVT(pollFn, doFn, blockFn) =>
			fun doFn' (k : cont(any) / exh : exh) : unit =
			      cont k' (x : any) =
				  let y : any = apply f (x / exh)
				  (* in *)
				    throw k' (y)
			      (* in *)
				apply doFn (k' / exh)
			fun blockFn' (flg : dirty_flag, tid : tid, k : cont(any) / exh : exh) : unit =
			      cont k' (x : any) =
				  let y : any = apply f (x / exh)
				  (* in *)
				    throw k' (y)
			      (* in *)
				apply blockFn (flg, tid, k' / exh)
			(* in *)
			  return (BEVT(pollFn, doFn', blockFn'))
		  end
	    (* in *)
	      apply wrapf (ev / exh)
	;
(*
	define inline @claim (flg : dirty_flag / exh : exh) : bool =
	    fun spin (_ : unit / exh : exh) : bool =
		  let sts : event_state = CAS(&0(flg), WAITING_EVT, SYNCHED_EVT)
		  (* in *)
		    case sts
		     of WAITING => apply spin (UNIT / exh)
		      | WAITING_EVT => return (true)
		      | SYNCHED_EVT => return (false)
		    end
	    (* in *)
	      apply spin (UNIT / exh)
	;

      (* record the calling thread's continuation in the event waiting queues *)
	define @block (evt : pevent / exh : exh) : any =
	    let fls : fls = @get-fls(host_vproc / exh)
	    cont resumeK (x : any) = return (x)
	    (* in *)
	      let flg : dirty_flag = alloc(INIT_EVT)
	      let blockArg : [dirty_flag, fls, cont(any)] = alloc(flg, fls, resumeK)
	      let blockArg : [dirty_flag, fls, cont(any)] = promote (blockArg)
	      fun block (ev : evt / exh : exh) : unit =
		    case ev
		     of BEVT(pollFn, doFn, blockFn) =>
			  apply blockFn(blockArg / exh)
		      | CHOOSE(ev1, ev2) =>
			  let (_ : unit) = apply block (ev1 / exh)
			  apply block (ev2 / exh)
		    end
	      let (_ : unit) = apply block (evt / exh)
	    (* if we get here, then we are ready to let other threads synchronize
	     * on this event.
	     *)
	      do #0(flg) := WAITING_EVT
	      (* in *)
		Threads.@thread-exit (/exh)
	;
  
      (* attempt to complete an enabled communication *)
	define @doEvent (enabled : list / exh : exh) : any =
	    cont syncK (x : any) = return (x)
	    (* in *)
	      fun doit (l : list / exh : exh) : any =
		    case l
		     of CONS(doFn : evt_do_fn, r : list) =>
			  let (_ : unit) = apply doFn (syncK / exh)
			(* if we get here, that means that the attempt failed, so try the next one *)
			  apply doit (r / exh)
		      | NIL => apply blockThd (UNIT / exh)
		    end
	      (* in *)
		apply doit (enabled / exh)
	;
*)
      )

    val always : 'a -> 'a event = _prim(@always)
    val never : 'a event = BEVT(fn () => false, fn _ => (), fn _ => ())
    val choose = CHOOSE
    val wrap : ('a event * ('a -> 'b)) -> 'b event = _prim(@wrap)

    val block : 'a pevent -> 'a = _prim (@block)
    val doEvent : ('a cont -> unit) list -> 'a = _prim (@doEvent)

    fun sync evt = (case poll (evt, [])
	   of [] => block evt
	    | enabled => doEvent enabled
	  (* end case *))

  (*************** signal variables **************)

    type signal_var = _prim (![bool, bool, list])

    _primcode (
	define inline @cvar-new (_ : unit / _ : exh) : signal_var =
	    let cv : signal_var = alloc(false, false, nil)
	    let cv : signal_var = promote (cv)
	    return cv
	  ;

      (* lock a condition variable *)
	define inline @cvar-lock (cv : signal_var / ) : unit =
	    fun spinLp () : () = if #0(cv) then
		    do Pause()
		    apply spinLp ()
		  else if TAS(&0(cv))
		    then apply spinLp ()
		    else return ()
	    apply spinLp ()
	  ;

	define inline @cvar-lock (cv : signal_var / ) : unit =

	define inline @cvar-signal (cv : signal_var / _ : exh) : unit =
	    let 
	  ;

	define inline @cvar-wait (cv : signal_var / _ : exh) : unit =
	  ;

	define inline @cvar-wait-evt (cv : signal_var / _ : exh) : unit =
	  ;
      )

    val signalVar : unit -> signal_var = _prim(@new-cv)
    val signal    : signal_var -> unit = _prim(@cvar-signal)
    val waitEvt   : signal_var -> unit pevent = _prim(@cvar-wait-evt)
    val wait      : signal_var -> unit = _prim(@cvar-wait)

  end


