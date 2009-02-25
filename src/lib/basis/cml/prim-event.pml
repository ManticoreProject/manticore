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

  end*) = struct

    type 'a cont = _prim (cont(any))

    datatype event_status = WAITING | CLAIMED | SYNCHED

  (* BOM types *)
    _primcode (
	typedef event_status = event_status;
	typedef event_state = ![event_status];
	typedef poll_fn = fun(unit / exh -> bool);
	typedef do_fn = fun(cont(any) / exh -> unit);
	typedef blk_fn = fun(event_state, FLS.fls, cont(any) / exh -> unit);
      )

    type event_state = _prim(event_state)
    type fls = _prim(FLS.fls)

    datatype 'a pevent
      = CHOOSE of ('a pevent * 'a pevent)
      | BEVT of (unit -> bool) * ('a cont -> unit) * ((event_state * fls * 'a cont) -> unit)

  (* optimistically poll the base events *)
    fun poll (evt, enabled) = (case evt
	   of CHOOSE(evt1, evt2) => poll (evt2, poll (evt1, enabled))
	    | BEVT(pollFn, doFn, blockFn) => if pollFn()
		then doFn::enabled
		else enabled
	  (* end case *))

    _primcode (
	typedef pevent = pevent;

	define inline @base-event (pollFn : poll_fn, doFn : do_fn, blockFn : blk_fn / _ : exh) : pevent =
	    return (BEVT(pollFn, doFn, blockFn))
	  ;

	define inline @always (x : any / _ : exh) : pevent =
	    fun pollFn (_ : unit / _ : exh) : bool = return (true)
	    fun doFn (k : cont(any) / _ : exh) : unit = throw k(x)
	    fun blockFn (_ : event_state, _ : FLS.fls, _ : cont(any) / exh : exh) : unit =
		  throw exh (UNIT) (* should never happen *)
	    (* in *)
	      return (BEVT(pollFn, doFn, blockFn))
	;

	define inline @never (_ : unit / _ : exh) : pevent =
	    fun pollFn (_ : unit / _ : exh) : bool = return (false)
	    fun doFn (k : cont(any) / exh : exh) : unit =
		  throw exh (UNIT) (* should never happen *)
	    fun blockFn (_ : event_state, _ : FLS.fls, _ : cont(any) / exh : exh) : unit =
		  return (UNIT)
	    (* in *)
	      return (BEVT(pollFn, doFn, blockFn))
	;

	define @wrap (ev : pevent, f : fun(any / exh -> any) / exh : exh) : pevent =
	    fun wrapf (ev : pevent / exh : exh) : pevent =
		  case ev
		   of CHOOSE(ev1 : pevent, ev2 : pevent) =>
			let ev1' : pevent = apply wrapf (ev / exh)
			let ev2' : pevent = apply wrapf (ev / exh)
			(* in *)
			  return (CHOOSE(ev1', ev2'))
		    | BEVT(pollFn : poll_fn, doFn : do_fn, blockFn : blk_fn) =>
			fun doFn' (k : cont(any) / exh : exh) : unit =
			      cont k' (x : any) =
				  let y : any = apply f (x / exh)
				  (* in *)
				    throw k' (y)
			      (* in *)
				apply doFn (k' / exh)
			fun blockFn' (flg : event_state, fls : FLS.fls, k : cont(any) / exh : exh) : unit =
			      cont k' (x : any) =
				  let y : any = apply f (x / exh)
				  (* in *)
				    throw k' (y)
			      (* in *)
				apply blockFn (flg, fls, k' / exh)
			(* in *)
			  return (BEVT(pollFn, doFn', blockFn'))
		  end
	    (* in *)
	      apply wrapf (ev / exh)
	;

      (* record the calling thread's continuation in the event waiting queues *)
	define @block (evt : pevent / exh : exh) : any =
	    let fls : FLS.fls = FLS.@get(host_vproc / exh)
	    cont resumeK (x : any) = return (x)
	    (* in *)
	      let flg : event_state = alloc(WAITING)
	      let blockArg : [event_state, FLS.fls, cont(any)] = alloc(flg, fls, resumeK)
	      let blockArg : [event_state, FLS.fls, cont(any)] = promote (blockArg)
	      fun block (ev : pevent / exh : exh) : unit =
		    case ev
		     of BEVT(pollFn : poll_fn, doFn : do_fn, blockFn : blk_fn) =>
			  apply blockFn(blockArg / exh)
		      | CHOOSE(ev1 : pevent, ev2 : pevent) =>
			  let (_ : unit) = apply block (ev1 / exh)
			  apply block (ev2 / exh)
		    end
	      let (_ : unit) = apply block (evt / exh)
	    (* if we get here, then we are ready to let other threads synchronize
	     * on this event.
	     *)
	      do #0(flg) := WAITING
	      (* in *)
		Threads.@thread-exit (/exh)
	;
  
      (* attempt to complete an enabled communication *)
	define @doEvent (enabled : list / exh : exh) : any =
	    cont syncK (x : any) = return (x)
	    (* in *)
	      fun doit (l : list / exh : exh) : any =
		    case l
		     of CONS(doFn : do_fn, r : list) =>
			  let (_ : unit) = apply doFn (syncK / exh)
			(* if we get here, that means that the attempt failed, so try the next one *)
			  apply doit (r / exh)
		      | nil => apply blockThd (UNIT / exh)
		    end
	      (* in *)
		apply doit (enabled / exh)
	;

      )

    val always : 'a -> 'a event = _prim(@always)
    val never : 'a event = _prim(@never)
    val choose = CHOOSE
    val wrap : ('a event * ('a -> 'b)) -> 'b event = _prim(@wrap)

    val block : 'a pevent -> 'a = _prim (@block)
    val doEvent : ('a cont -> unit) list -> 'a = _prim (@doEvent)

    fun sync evt = (case poll (evt, nil)
	   of nil => block evt
	    | enabled => doEvent enabled
	  (* end case *))

  end


