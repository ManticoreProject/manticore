(* prim-event.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimEvent : sig

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
    val waitEvt : signal_var -> unit pvevent
    val wait : signal_var -> unit

  end = struct

    type 'a cont = _prim (cont(any))
    type flag = _prim(![bool])

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
    (* record the calling thread's continuation in the event waiting queues *)
      define @block (evt : pevent / exh : exh) : any =
	  let fgs : fgs = @get-fgs(host_vproc / exh)
	  cont resumeK (x : any) = return (x)
	  (* in *)
	    let flg : dirty_flag = alloc(INIT_EVT)
	    let blockArg : [dirty_flag, fgs, cont(any)] = alloc(flg, fgs, resumeK)
	    let blockArg : [dirty_flag, fgs, cont(any)] = promote (blockArg)
	    fun block (ev : evt / exh : exh) : unit =
		  case ev
		   of BEVT_PAT(pollFn, doFn, blockFn) =>
			apply blockFn(blockArg / exh)
		    | CHOOSE_PAT(ev1, ev2) =>
			let (_ : unit) = apply block (ev1 / exh)
			apply block (ev2 / exh)
		  end
	    let (_ : unit) = apply block (evt / exh)
	  (* if we get here, then we are ready to let other threads synchronize
	   * on this event.
	   *)
	    do #0(flg) := WAITING_EVT
	    (* in *)
	      @thread-exit(/exh)
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
    )

    val block : 'a pevent -> 'a = _primfn 
    val block : 'a pevent -> 'a = _prim (@block)
    val doEvent : ('a cont -> unit) list -> 'a = _prim (@doEvent)

    fun sync evt = (case poll (evt, [])
	   of [] => block evt
	    | enabled => doEvent enabled
	  (* end case *))

  end


