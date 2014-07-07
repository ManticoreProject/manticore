(* chan.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * CML channels.
 *)

structure Chan : sig

    type 'a chan
    val new : unit -> 'a chan
    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

  end = struct

    structure FLS = FiberLocalStorage

    _primcode (

      (* the representation of a CML thread suspended on a channel *)
	typedef recvq_item = [
	    PrimEvent.dirty_flag,	(* event-instance status flag *)
	    FLS.fls,			(* FLS of thread *)
	    vproc,			(* vproc affinity *)
	    cont(any)			(* thread's continuation *)
	  ];
	typedef sendq_item = [
	    PrimEvent.dirty_flag,	(* event-instance status flag *)
	    FLS.fls,			(* FLS of thread *)
	    vproc,			(* vproc affinity *)
	    cont(unit)			(* thread's continuation *)
	  ];

	typedef chan_rep = ![	    (* all fields are mutable *)
	    int,			(* spin lock *)
	    List.list,			(* sendq head *)
	    List.list,			(* sendq tail *)
	    List.list,			(* recvq head *)
	    List.list			(* recvq tail *)
	  ];
#	define CH_TRY_LOCK(ch)		BCAS(&0(ch), false, true)
#	define CH_CLR_LOCK(ch)		UPDATE(0, ch, true)
#	define CH_GET_SENDQ_HD(ch)	SELECT(1, ch)
#	define CH_SET_SENDQ_HD(ch,x)	UPDATE(1, ch, x)
#	define CH_GET_SENDQ_TL(ch)	SELECT(2, ch)
#	define CH_SET_SENDQ_TL(ch,x)	UPDATE(2, ch, x)
#	define CH_GET_RECVQ_HD(ch)	SELECT(3, ch)
#	define CH_SET_RECVQ_HD(ch,x)	UPDATE(3, ch, x)
#	define CH_GET_RECVQ_TL(ch)	SELECT(4, ch)
#	define CH_SET_RECVQ_TL(ch,x)	UPDATE(4, ch, x)

	define inline @throw-to (fls : FLS.fls, recv : cont(any), v : any / exh : exh) noreturn =
	  do FLS.@set (fls)
	  do VProc.@atomic-end ()
	  throw recv (v)
	;

	define inline @chan-acquire-lock (ch : chan_rep / exh : exh) : () =
	    fun spin () : () =
		  if I32isSet(&0(ch)) then
		    do Pause ()
		    apply spin ()
		  else if I32TAS(&(ch)) then
		    do Pause ()
		    apply spin ()
		  else
		    return ()
	    (* in *)
	      apply spin ()
	  ;

	define inline @chan-release-lock (ch : chan_rep / _ : exh) : () =
	    do #0(ch) := false
	    return ()
	  ;

	define inline @chan-enqueue-recv (ch : chan_rep, flg : dirty_flag, tid : FLS.fls, k : cont(any) / _ : exh) : () =
	    let item : recvq_item = alloc (flg, tid, host_vproc, k)
	    let cons : List.list = List.CONS(item, CH_GET_RECVQ_TL(ch))
	    let l : List.list = promote (cons)
	    do CH_SET_RECVQ_TL(ch, l)
	      return ()
	;

      (* out-of-line version for when we must reverse the tail *)
	define @chan-dequeue-recv-slowpath (ch : chan_rep / exh : exh) : Option.option =
	  (* reverse the tail of the queue *)
	    fun rev (item : recvq_item, tl : List.list, hd : List.list / exh : exh) : Option.option =
		  case tl
		   of nil => (* update head of queue and return item *)
			let hd : List.list = promote(hd)
			do CH_SET_RECVQ_HD(ch, hd)
			let result : Option.option = Option.SOME(item)
			(* in *)
			  return (result)
		    | CONS(item' : recvq_item, rest : List.list) =>
			let hd : List.list = List.CONS(item, hd)
			(* in *)
			  apply rev(item', rest, hd / exh)
		  end
	    let tl : List.list = CH_GET_RECVQ_TL(ch)
	    (* in *)
	      case tl
	       of List.CONS(item : recvq_item, rest : List.list) =>
		    do CH_SET_RECVQ_TL(ch, nil)
		      apply rev (item, rest, nil / exh)
		| nil => return (Option.NONE)
	      end
	;

	define inline @chan-dequeue-recv (ch : chan_rep / exh : exh) : Option.option =
	  (* first, try the head of the queue *)
	    case CH_GET_RECVQ_HD(ch)
	     of nil => @chan-dequeue-recv-slowpath(ch / exh)
	      | List.CONS(item : recvq_item, rest : List.list) =>
		  do CH_SET_RECVQ_HD(ch, rest)
		  let result : Option.option = Option.SOME(item)
		  (* in *)
		    return (result)
	    end
	;

	define inline @chan-enqueue-send (ch : chan_rep, msg : any, tid : FLS.fls, k : cont(unit) / _ : exh) : () =
	    let item : sendq_item = alloc (msg, tid, host_vproc, k)
	    let cons : List.list = List.CONS(item, CH_GET_SENDQ_TL(ch))
	    let l : List.list = promote (cons)
	    do CH_SET_SENDQ_TL(ch, l)
	      return ()
	;

      (* out-of-live version for when we must reverse the tail *)
	define @chan-dequeue-send-slowpath (ch : chan_rep / exh : exh) : Option.option =
	  (* reverse the tail of the queue *)
	    fun rev (item : sendq_item, tl : List.list, hd : List.list / exh : exh) : Option.option =
		  case tl
		   of nil => (* update head of queue and return item *)
			let hd : List.list = promote(hd)
			do CH_SET_SENDQ_HD(ch, hd)
			let result : Option.option = Option.SOME(item)
			(* in *)
			  return (result)
		    | List.CONS(item' : sendq_item, rest : List.list) =>
			let hd : List.list = List.CONS(item, hd)
			(* in *)
			  apply rev(item', rest, hd / exh)
		  end
	    let tl : List.list = CH_GET_SENDQ_TL(ch)
	    (* in *)
	      case tl
	       of List.CONS(item : sendq_item, rest : List.list) =>
		    do CH_SET_SENDQ_TL(ch, nil)
		      apply rev (item, rest, nil / exh)
		| nil => return (Option.NONE)
	      end
	;
	
	define inline @chan-dequeue-send (ch : chan_rep / exh : exh) : Option.option =
	  (* first, try the head of the queue *)
	    case CH_GET_SENDQ_HD(ch)
	     of nil => @chan-dequeue-send-slowpath(ch / exh)
	      | List.CONS(item : sendq_item, rest : List.list) =>
		  do CH_SET_SENDQ_HD(ch, rest)
		  let result : Option.option = Option.SOME(item)
		  (* in *)
		    return (result)
	    end
	;

	define inline constr @chan-new (arg : unit / exh : exh) : chan_rep =
	    let ch : chan_rep = alloc(false, nil, nil, nil, nil)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	;
	
	define @chan-recv (ch : chan_rep / exh : exh) : any =
	    let fls : FLS.fls = FLS.@get()
	    do VProc.@atomic-begin ()
	    do @chan-acquire-lock (ch / exh)
	    let maybeItem : Option.option = @chan-dequeue-send (ch / exh)
	    (* in *)
	      case maybeItem
	       of Option.SOME(item : sendq_item) =>
		    do @chan-release-lock (ch / exh)
		    do VProcQueue.@enqueue-on-vproc(#2(item), #1(item), #3(item))
		    do VProc.@atomic-end ()
		    (* in *)
		      return (#0(item))
		| Option.NONE =>
		    cont recvK (x : any) = return (x)
		    (* in *)
		      let flag : dirty_flag = alloc(WAITING_EVT)
		      let flag : dirty_flag = promote (flag)
		      do @chan-enqueue-recv (ch, flag, fls, recvK / exh)
		      do @chan-release-lock (ch / exh)
		      do VProc.@atomic-end ()
		      (* in *)
			SchedulerAction.@stop()
	      end
	;
    
	define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
	    let ch : chan_rep = #0(arg)
	    let msg : any = #1(arg)
	    let fls : FLS.fls = FLS.@get()
	    do VProc.@atomic-begin ()
	    do @chan-acquire-lock (ch / exh)
	    cont sendK (x : unit) = return (x)
	    (* in *)
	      fun tryLp (_ : unit / exh : exh) : any =
		    let maybeItem : Option.option = @chan-dequeue-recv(ch / exh)
		    (* in *)
		      case maybeItem
		       of Option.SOME(item : recvq_item) =>
			  (* there is a matching recv, but we must check to make sure
			   * that some other thread has not already claimed the event.
			   *)
			    let success : bool = PrimEvent.@claim (#0(item) / exh)
			    (* in *)
			      if success then (* we got it *)
				do @chan-release-lock(ch / exh)
				do VProcQueue.@enqueue-on-vproc(#2(item), #1(item), #3(item))
				(* in *)
				  @throw-to (#1(item), #3(item), msg / exh)
			      else (* someone else got the event, so try again *)
				apply tryLp (UNIT / exh)
			| Option.NONE =>
			    do @chan-enqueue-send (ch, msg, fls, sendK / exh)
			    do @chan-release-lock (ch / exh)
			    do VProc.@atomic-end ()
			    (* in *)
			      SchedulerAction.@stop()
		      end
	      (* in *)
		apply tryLp (UNIT / exh)
	;
    
      )

    type 'a chan = _prim (chan_rep)

    val new : unit -> 'a chan		= _prim (@chan-new)
    val send : ('a chan * 'a) -> unit	= _prim (@chan-send)
    val recv : 'a chan -> 'a		= _prim (@chan-recv)

    val sendPrimEvt : ('a chan * 'a) -> unit PrimEvent.pevent	= _prim (@chan-send-evt)
    val recvPrimEvt : 'a chan -> 'a PrimEvent.pevent		= _prim (@chan-recv-evt)

    fun recvEvt ch = Event.baseEvt (recvPrimEvt ch)
    fun sendEvt (ch, msg) = Event.baseEvt (sendPrimEvt(ch, msg))

  end
