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
	typedef poll_fn = fun( / -> bool);
	typedef do_fn = fun(vproc, cont(any) / exh -> );
	typedef blk_fn = fun(vproc, event_state, FLS.fls, cont(any) / exh -> );
      )

    type event_state = _prim(event_state)
    type poll_fn = _prim(poll_fn)
    type do_fn = _prim(do_fn)
    type blk_fn = _prim(blk_fn)

    datatype 'a pevent
      = CHOOSE of ('a pevent * 'a pevent)
      | BEVT of (poll_fn * do_fn * blk_fn)

    _primcode (
	typedef pevent = pevent;

	define inline @base-event (pollFn : poll_fn, doFn : do_fn, blockFn : blk_fn / _ : exh) : pevent =
	    return (BEVT(pollFn, doFn, blockFn))
	  ;

	define inline @always (x : any / _ : exh) : pevent =
	    fun pollFn () : bool = return (true)
	    fun doFn (_ : vproc, k : cont(any) / _ : exh) : () = throw k(x)
	    fun blockFn (_ : vproc, _ : event_state, _ : FLS.fls, _ : cont(any) / exh : exh) : () =
		  return () (* should never happen *)
	    (* in *)
	      return (BEVT(pollFn, doFn, blockFn))
	  ;

	define inline @never (_ : unit / _ : exh) : pevent =
	    fun pollFn () : bool = return (false)
	    fun doFn (_ : vproc, k : cont(any) / exh : exh) : () =
		  throw exh (UNIT) (* should never happen *)
	    fun blockFn (_ : vproc, _ : event_state, _ : FLS.fls, _ : cont(any) / exh : exh) : () =
		  return ()
	    (* in *)
	      return (BEVT(pollFn, doFn, blockFn))
	  ;

	define inline @wrap (arg : [pevent, fun(any / exh -> any)] / exh : exh) : pevent =
	    let f : fun(any / exh -> any) = #1(arg)
	    fun wrapf (ev : pevent / exh : exh) : pevent =
		  case ev
		   of CHOOSE(ev1 : pevent, ev2 : pevent) =>
			let ev1' : pevent = apply wrapf (ev1 / exh)
			let ev2' : pevent = apply wrapf (ev2 / exh)
			(* in *)
			  return (CHOOSE(ev1', ev2'))
		    | BEVT(pollFn : poll_fn, doFn : do_fn, blockFn : blk_fn) =>
			fun doFn' (vp : vproc, k : cont(any) / exh : exh) : () =
			      cont k' (x : any) =
				  let y : any = apply f (x / exh)
				  (* in *)
				    throw k (y)
			      (* in *)
				apply doFn (vp, k' / exh)
			fun blockFn' (vp : vproc, flg : event_state, fls : FLS.fls, k : cont(any) / exh : exh) : () =
			      cont k' (x : any) =
				  let y : any = apply f (x / exh)
				  (* in *)
				    throw k (y)
			      (* in *)
				apply blockFn (vp, flg, fls, k' / exh)
			(* in *)
			  return (BEVT(pollFn, doFn', blockFn'))
		  end
	    (* in *)
	      apply wrapf (#0(arg) / exh)
	  ;

	define @sync (evt : pevent / exh : exh) : any =
	    cont resumeK (x : any) = return(x)
(* NOTE: we might want to restrict the scope of the atomic-begin/end to individual base events *)
	    let self : vproc = SchedulerAction.@atomic-begin()
	    (* in *)
	    (* *)
	      fun blockThd () : any =
		    let flg : event_state = alloc (WAITING)
		    let fls : FLS.fls = FLS.@get-in-atomic(self)		    
		    fun block (evt : pevent) : () =
			  case evt
			   of CHOOSE(evt1 : pevent, evt2 : pevent) =>
				do apply block (evt1)
				apply block (evt2)
			    | BEVT(_ : poll_fn, _ : do_fn, blkFn : blk_fn) =>
				apply blkFn (self, flg, fls, resumeK / exh)
			  end
		    do apply block (evt)
		    (* in *)
		      SchedulerAction.@stop-from-atomic (self)
	    (* attempt to synchronize on an enabled event *)
	      fun doEvt (enabled : List.list) : any =
		    case enabled
		     of nil => apply blockThd ()
		      | CONS(doFn : do_fn, r : List.list) =>
			  do apply doFn (self, resumeK / exh)
			  (* in *)
			    apply doEvt (r)
		    end
	    (* optimistically poll for enabled base events *)
	      fun poll (evt : pevent, enabled : List.list) : List.list =
		    case evt
		     of CHOOSE(evt1 : pevent, evt2 : pevent) =>
			  let l : List.list = apply poll(evt1, enabled)
			    apply poll(evt2, l)
		      | BEVT(pollFn : poll_fn, doFn : do_fn, _ : blk_fn) =>
			  let b : bool = apply pollFn ()
			  (* in *)
			    if b
			      then return (CONS(doFn, enabled))
			      else return (enabled)
		    end
	      let enabled : List.list = apply poll (evt, nil)
	      (* in *)
		apply doEvt (enabled)
	    ;

      )

    val always : 'a -> 'a pevent = _prim(@always)
(* FIXME: the CML type of never should be 'a pevent, but we do not have a way to create a
 * polymorphic value in BOM!
 *)
    val never : unit -> 'a pevent = _prim(@never)
    val choose = CHOOSE
    val wrap : ('a pevent * ('a -> 'b)) -> 'b pevent = _prim(@wrap)
    val sync : 'a pevent -> 'a = _prim(@sync)

  end


