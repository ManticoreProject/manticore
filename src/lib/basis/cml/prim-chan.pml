(* prim-chan.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *)

#include "spin-lock.def"

structure PrimChan : sig

    type 'a chan

    val new : unit -> 'a chan

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

(*
    val sendEvt : ('a chan * 'a) -> unit PrimEvent.pevent
    val recvEvt : 'a chan -> 'a PrimEvent.pevent
*)

  end = struct

    structure PT = PrimTypes
    structure PEvt = PrimEvent

    _primcode (

      (* the representation of a CML thread suspended on a channel *)
	typedef recvq_item = [
	    PEvt.event_state,		(* 0: event-instance status flag *)
	    vproc,			(* 1: vproc affinity *)
	    FLS.fls,			(* 2: FLS of thread *)
	    cont(any)			(* 3: thread's continuation *)
	  ];
	typedef sendq_item = [
	    PEvt.event_state,		(* 0: event-instance status flag *)
	    any,			(* 1: message *)
	    vproc,			(* 2: vproc affinity *)
	    FLS.fls,			(* 3: FLS of thread *)
	    cont(unit)			(* 4: thread's continuation *)
	  ];

	typedef chan_rep = ![	    (* all fields are mutable *)
	    bool,			(* spin lock *)
	    List.list,			(* sendq head *)
	    List.list,			(* sendq tail *)
	    List.list,			(* recvq head *)
	    List.list			(* recvq tail *)
	  ];

	(* offsets into the chan_rep object *)
#	define CH_LOCK		0
#	define CH_SENDQ_HD	1
#	define CH_SENDQ_TL	2
#	define CH_RECVQ_HD	3
#	define CH_RECVQ_TL	4

      (* enqueue an item on a channel's recv queue *)
	define inline @chan-enqueue-recv (ch : chan_rep, flg : PEvt.event_state, vp : vproc, fls : FLS.fls, k : cont(any)) : () =
	    let item : recvq_item = alloc (flg, vp, fls, k)
	    let cons : List.list = List.CONS(item, SELECT(CH_RECVQ_TL, ch))
	    let l : List.list = promote (cons)
	    do UPDATE(CH_RECVQ_TL, ch, l)
	      return ()
	;

      (* out-of-line version for when we must reverse the tail *)
	define @chan-dequeue-recv-slowpath (ch : chan_rep) : Option.option =
	  (* reverse the tail of the queue *)
	    fun rev (item : recvq_item, tl : List.list, hd : List.list) : Option.option =
		  case tl
		   of nil => (* update head of queue and return item *)
			let hd : List.list = promote(hd)
			do UPDATE(CH_RECVQ_HD, ch, hd)
			let result : Option.option = Option.SOME(item)
			(* in *)
			  return (result)
		    | CONS(item' : recvq_item, rest : List.list) =>
			let hd : List.list = List.CONS(item, hd)
			(* in *)
			  apply rev(item', rest, hd)
		  end
	    let tl : List.list = SELECT(CH_RECVQ_TL, ch)
	    (* in *)
	      case tl
	       of List.CONS(item : recvq_item, rest : List.list) =>
		    do UPDATE(CH_RECVQ_TL, ch, nil)
		      apply rev (item, rest, nil)
		| nil => return (Option.NONE)
	      end
	;

	define inline @chan-dequeue-recv (ch : chan_rep) : Option.option =
	  (* first, try the head of the queue *)
	    case SELECT(CH_RECVQ_HD, ch)
	     of nil => @chan-dequeue-recv-slowpath(ch)
	      | List.CONS(item : recvq_item, rest : List.list) =>
		  do UPDATE(CH_RECVQ_HD, ch, rest)
		  let result : Option.option = Option.SOME(item)
		  (* in *)
		    return (result)
	    end
	;

	define inline @chan-enqueue-send (ch : chan_rep, flg : PEvt.event_state, msg : any, vp : vproc, fls : FLS.fls, k : cont(unit)) : () =
	    let item : sendq_item = alloc (flg, msg, vp, fls, k)
	    let cons : List.list = List.CONS(item, SELECT(CH_SENDQ_TL, ch))
	    let l : List.list = promote (cons)
	    do UPDATE(CH_SENDQ_TL, ch, l)
	      return ()
	;

      (* out-of-live version for when we must reverse the tail *)
	define @chan-dequeue-send-slowpath (ch : chan_rep) : Option.option =
	  (* reverse the tail of the queue *)
	    fun rev (item : sendq_item, tl : List.list, hd : List.list) : Option.option =
		  case tl
		   of nil => (* update head of queue and return item *)
			let hd : List.list = promote(hd)
			do UPDATE(CH_SENDQ_HD, ch, hd)
			let result : Option.option = Option.SOME(item)
			(* in *)
			  return (result)
		    | List.CONS(item' : sendq_item, rest : List.list) =>
			let hd : List.list = List.CONS(item, hd)
			(* in *)
			  apply rev(item', rest, hd)
		  end
	    let tl : List.list = SELECT(CH_SENDQ_TL, ch)
	    (* in *)
	      case tl
	       of List.CONS(item : sendq_item, rest : List.list) =>
		    do UPDATE(CH_SENDQ_TL, ch, nil)
		      apply rev (item, rest, nil)
		| nil => return (Option.NONE)
	      end
	  ;
	
	define inline @chan-dequeue-send (ch : chan_rep) : Option.option =
	  (* first, try the head of the queue *)
	    case SELECT(CH_SENDQ_HD, ch)
	     of nil => @chan-dequeue-send-slowpath(ch)
	      | List.CONS(item : sendq_item, rest : List.list) =>
		  do UPDATE(CH_SENDQ_HD, ch, rest)
		  let result : Option.option = Option.SOME(item)
		  (* in *)
		    return (result)
	    end
	;

	define inline @chan-new (arg : unit / exh : exh) : chan_rep =
	    let ch : chan_rep = alloc(false, nil, nil, nil, nil)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	;
	
	define @chan-recv (ch : chan_rep / exh : exh) : any =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    SPIN_LOCK(ch, CH_LOCK)
	  (* a loop to try to get an item from the sendq *)
	    fun tryLp () : any =
		  let maybeItem : Option.option = @chan-dequeue-send (ch)
		  (* in *)
		    case maybeItem
		     of Option.SOME(item : sendq_item) =>
			  let state : PEvt.event_state = #0(item)
			  if Equal(DEREF(state), PEvt.SYNCHED)
			    then apply tryLp()
			    else
			    (* there is a matching send, but we must
			     * check to make sure that some other
			     * thread has not already claimed the event.
			     *)
			      fun matchLp () : any =
				    let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
				    case sts
				     of PEvt.WAITING => (* we matched the send! *)
					  SPIN_UNLOCK(ch, CH_LOCK)
					  do Threads.@enqueue-ready-in-atomic (
					      self, #2(item), #3(item), #4(item))
					  do SchedulerAction.@atomic-end (self)
					  (* in *)
					    return (#1(item))
				      | PEvt.CLAIMED => (* may be claimed, so spin *)
					  do Pause()
					  apply matchLp()
				      | PEvt.SYNCHED => (* some other thread got it *)
					  apply tryLp()
				    end
			      (* in *)
				apply matchLp ()
		      | Option.NONE =>
			  cont recvK (x : any) = return (x)
			  (* in *)
			    let fls : FLS.fls = FLS.@get()
			    let flg : PEvt.event_state = alloc(PEvt.WAITING)
			    do @chan-enqueue-recv (ch, flg, self, fls, recvK)
			    SPIN_UNLOCK(ch, CH_LOCK)
			    (* in *)
			      SchedulerAction.@stop-from-atomic(self)
		    end
	    (* in *)
	      apply tryLp ()
	  ;
    
	define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
	    let ch : chan_rep = #0(arg)
	    let msg : any = #1(arg)
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    SPIN_LOCK(ch, CH_LOCK)
	    fun tryLp () : unit =
		  let maybeItem : Option.option = @chan-dequeue-recv(ch)
		  (* in *)
		    case maybeItem
		     of Option.SOME(item : recvq_item) =>
			  let state : PEvt.event_state = #0(item)
			  if Equal(DEREF(state), PEvt.SYNCHED)
			    then apply tryLp()
			    else
			    (* there is a matching recv, but we must
			     * check to make sure that some other
			     * thread has not already claimed the event.
			     *)
			      fun matchLp () : unit =
				    let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
				    case sts
				     of PEvt.WAITING => (* we matched the recv! *)
					  SPIN_UNLOCK(ch, CH_LOCK)
					  if Equal(self, #1(item))
					    then (* sending to a local thread *)
					      cont sendK (_ : unit) = return (UNIT)
					      (* in *)
						let fls : FLS.fls = FLS.@get-in-atomic(self)
						do VProcQueue.@enqueue-in-atomic (self, fls, sendK)
						do FLS.@set-in-atomic(self, #2(item))
						do SchedulerAction.@atomic-end (self)
						let k : cont(any) = #3(item)
						(* in *)
						  throw k (msg)
					    else (* sending to a remote thread *)
					      do SchedulerAction.@atomic-end (self)
					      let k : cont(any) = #3(item)
					      cont recvk (_ : unit) = throw k (msg)
					      (* in *)
						do VProcQueue.@enqueue-on-vproc (#1(item), #2(item), recvk)
						return (UNIT)
				      | PEvt.CLAIMED => (* may be claimed, so spin *)
					  do Pause()
					  apply matchLp()
				      | PEvt.SYNCHED => (* some other thread got it *)
					  apply tryLp()
				    end
			      (* in *)
				apply matchLp ()
			| Option.NONE =>
			    cont sendK (_ : unit) = return (UNIT)
			    (* in *)
			      let fls : FLS.fls = FLS.@get-in-atomic(self)
			      let flg : PEvt.event_state = alloc(PEvt.WAITING)
			      do @chan-enqueue-send (ch, flg, msg, self, fls, sendK)
			      SPIN_UNLOCK(ch, CH_LOCK)
			      (* in *)
				SchedulerAction.@stop-from-atomic(self)
		      end
	      (* in *)
		apply tryLp ()
	;
    
      )

    type 'a chan = _prim (chan_rep)

    val new : unit -> 'a chan		= _prim (@chan-new)
    val send : ('a chan * 'a) -> unit	= _prim (@chan-send)
    val recv : 'a chan -> 'a		= _prim (@chan-recv)

(*
    val sendEvt : ('a chan * 'a) -> unit PrimEvent.pevent	= _prim (@chan-send-evt)
    val recvEvt : 'a chan -> 'a PrimEvent.pevent		= _prim (@chan-recv-evt)
*)

  end
