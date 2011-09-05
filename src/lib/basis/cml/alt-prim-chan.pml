(* prim-chan.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *
 * This is an alternative implementation that uses an imperative representation of
 * the channel queues.
 *)

#include "spin-lock.def"

structure PrimChan (*: sig

    type 'a chan

    val new : unit -> 'a chan

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

(*
    val sendEvt : ('a chan * 'a) -> unit PrimEvent.pevent
    val recvEvt : 'a chan -> 'a PrimEvent.pevent
*)

  end*) = struct

    structure PT = PrimTypes
    structure PEvt = PrimEvent

    _primcode (

      (* the representation of a CML thread suspended on a channel *)
	typedef sendq_item = ![
	    PEvt.event_state,		(* 0: event-instance status flag *)
	    any,			(* 1: message *)
	    vproc,			(* 2: vproc affinity *)
	    FLS.fls,			(* 3: FLS of thread *)
	    cont(unit),			(* 4: thread's continuation *)
	    any				(* 5: link field *)
	  ];

	typedef recvq_item = ![
	    PEvt.event_state,		(* 0: event-instance status flag *)
	    vproc,			(* 1: vproc affinity *)
	    FLS.fls,			(* 2: FLS of thread *)
	    cont(any),			(* 3: thread's continuation *)
	    any				(* 4: link field *)
	  ];

	typedef chan_rep = ![	    (* all fields are mutable *)
	    int,			(* spin lock *)
	    sendq_item,			(* sendq head item *)
	    sendq_item,			(* sendq tail item *)
	    recvq_item,			(* recvq head item *)
	    recvq_item			(* recvq tail item *)
	  ];

	(* offsets into the chan_rep object *)
#	define CH_LOCK		0
#	define CH_SENDQ_HD	1
#	define CH_SENDQ_TL	2
#	define CH_RECVQ_HD	3
#	define CH_RECVQ_TL	4

	(* offsets in sendq items *)
#	define SENDQ_MSG	1
#	define SENDQ_VPROC	2
#	define SENDQ_FLS	3
#	define SENDQ_CONT	4
#	define SENDQ_LINK	5

	(* offsets in recvq items *)
#	define RECVQ_VPROC	1
#	define RECVQ_FLS	2
#	define RECVQ_CONT	3
#	define RECVQ_LINK	4

#	define Q_NIL	enum(0)

      (* does a channel have waiting receivers? *)
	define inline @chan-waiting-recv (ch : chan_rep) : bool =
	    if NotEqual(SELECT(CH_RECVQ_HD, ch), Q_NIL) then return (true) else return (false)
	  ;

      (* does a channel have waiting senders? *)
	define inline @chan-waiting-send (ch : chan_rep) : bool =
	    if NotEqual(SELECT(CH_SENDQ_HD, ch), Q_NIL) then return (true) else return (false)
	  ;

      (* enqueue an item on a channel's recv queue *)
	define inline @chan-enqueue-recv (ch : chan_rep, flg : PEvt.event_state, vp : vproc, fls : FLS.fls, k : cont(any)) : () =
	    let item : recvq_item = alloc (flg, vp, fls, k, Q_NIL)
	    let item : recvq_item = promote (item)
	    let tl : recvq_item = SELECT(CH_RECVQ_TL, ch)
	    if Equal(tl, Q_NIL)
	      then
		do UPDATE(CH_RECVQ_HD, ch, item)
		do UPDATE(CH_RECVQ_TL, ch, item)
		return ()
	      else
		do UPDATE(RECVQ_LINK, tl, (any)item)
		do UPDATE(CH_RECVQ_TL, ch, item)
		return ()
	  ;

      (* enqueue an item on a channel's send queue *)
	define inline @chan-enqueue-send (ch : chan_rep, flg : PEvt.event_state, msg : any, vp : vproc, fls : FLS.fls, k : cont(any)) : () =
	    let item : sendq_item = alloc (flg, msg, vp, fls, k, Q_NIL)
	    let item : sendq_item = promote (item)
	    let tl : sendq_item = SELECT(CH_SENDQ_TL, ch)
	    if Equal(tl, Q_NIL)
	      then
		do UPDATE(CH_SENDQ_HD, ch, item)
		do UPDATE(CH_SENDQ_TL, ch, item)
		return ()
	      else
		do UPDATE(SENDQ_LINK, tl, (any)item)
		do UPDATE(CH_SENDQ_TL, ch, item)
		return ()
	  ;

      (* dequeue an item from a channel's recv queue; returns Q_NIL if queue is empty *)
	define inline @chan-dequeue-recv (ch : chan_rep) : recvq_item =
	    let hd : recvq_item = SELECT(CH_RECVQ_HD, ch)
	    if Equal(hd, Q_NIL)
	      then
		return ((recvq_item)Q_NIL)
	      else
		let next : recvq_item = SELECT(RECVQ_LINK, hd)
		do UPDATE(CH_RECVQ_HD, ch, next)
		if Equal(next, Q_NIL)
		  then
		    do UPDATE(CH_RECVQ_TL, ch, next)
		    return (hd)
		  else return (hd)
	  ;

      (* dequeue an item from a channel's send queue; returns Q_NIL if queue is empty *)
	define inline @chan-dequeue-send (ch : chan_rep) : sendq_item =
	    let hd : sendq_item = SELECT(CH_SENDQ_HD, ch)
	    if Equal(hd, Q_NIL)
	      then
		return ((sendq_item)Q_NIL)
	      else
		let next : sendq_item = SELECT(SENDQ_LINK, hd)
		do UPDATE(CH_SENDQ_HD, ch, next)
		if Equal(next, Q_NIL)
		  then
		    do UPDATE(CH_SENDQ_TL, ch, next)
		    return (hd)
		  else return (hd)
	  ;

      (* remove the first item from the receive queue that does not have the same event
       * event-state object.  Return Q_NIL if there is no such item.
       *)
	define @chan-dequeue-recv-match (id : PEvt.event_state, ch : chan_rep) : recvq_item =
	    fun find (prev : recvq_item, item : recvq_item) : recvq_item =
		  if Equal(item, Q_NIL)
		    then return (item)
		    else
		      let next : recvq_item = SELECT(RECVQ_LINK, item)
		      if NotEqual(id, #0(item))
			then (* item has a different event_state, so we can match it *)
			(* first take care of the queue head *)
			  do if Equal(prev, Q_NIL)
			    then (* item was the head of the queue *)
			      do UPDATE(CH_RECVQ_HD, ch, next)
			      return ()
			    else
			      do UPDATE(RECVQ_LINK, prev, (any)next)
			      return ()
			(* take care of the tail *)
			  do if Equal(next, Q_NIL)
			    then (* item was the last one in the queue *)
			      do UPDATE(CH_RECVQ_TL, ch, prev)
			      return ()
			    else
			      return ()
			  return (item)
			else (* item has same event state, so continue looking *)
			  apply find (item, next)
	    apply find ((recvq_item)Q_NIL, SELECT(CH_RECVQ_HD, ch))
	  ;

      (* remove the first item from the send queue that does not have the same event
       * event-state object.  Return Q_NIL if there is no such item.
       *)
	define @chan-dequeue-send-match (id : PEvt.event_state, ch : chan_rep) : sendq_item =
	    fun find (prev : sendq_item, item : sendq_item) : sendq_item =
		  if Equal(item, Q_NIL)
		    then return (item)
		    else
		      let next : sendq_item = SELECT(CH_SENDQ_HD, item)
		      if NotEqual(id, #0(item))
			then (* item has a different event_state, so we can match it *)
			(* first take care of the queue head *)
			  do if Equal(prev, Q_NIL)
			    then (* item was the head of the queue *)
			      do UPDATE(CH_SENDQ_HD, ch, next)
			      return ()
			    else
			      do UPDATE(SENDQ_LINK, prev, (any)next)
			      return ()
			(* take care of the tail *)
			  do if Equal(next, Q_NIL)
			    then (* item was the last one in the queue *)
			      do UPDATE(CH_SENDQ_TL, ch, prev)
			      return ()
			    else
			      return ()
			  return (item)
			else (* item has same event state, so continue looking *)
			  apply find (item, next)
	    apply find ((sendq_item)Q_NIL, SELECT(CH_SENDQ_HD, ch))
	  ;

      (* push an item onto the front of a channel's recv queue *)
	define inline @chan-push-recv (ch : chan_rep, item : recvq_item) : () =
	    let hd : recvq_item = SELECT(CH_RECVQ_HD, ch)
	    do UPDATE(RECVQ_LINK, item, (any)hd)
	    do UPDATE(CH_RECVQ_HD, ch, item)
	    if Equal(hd, Q_NIL)
	      then
		do UPDATE(CH_RECVQ_TL, ch, item)
		return ()
	      else
		return ()
	  ;

      (* push an item onto the front of a channel's send queue *)
	define inline @chan-push-send (ch : chan_rep, item : sendq_item) : () =
	    let hd : sendq_item = SELECT(CH_SENDQ_HD, ch)
	    do UPDATE(SENDQ_LINK, item, (any)hd)
	    do UPDATE(CH_SENDQ_HD, ch, item)
	    if Equal(hd, Q_NIL)
	      then
		do UPDATE(CH_SENDQ_TL, ch, item)
		return ()
	      else
		return ()
	  ;

      (***** Channel operations *****)
	
	define inline constr @chan-new (arg : unit / exh : exh) : chan_rep =
	    let ch : chan_rep = alloc(0, (sendq_item)Q_NIL, (sendq_item)Q_NIL, (recvq_item)Q_NIL, (recvq_item)Q_NIL)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	  ;
	
	define @chan-recv (ch : chan_rep / exh : exh) : any =
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    SPIN_LOCK(ch, CH_LOCK)
	  (* a loop to try to get an item from the sendq *)
	    fun tryLp () : any =
		  let item : sendq_item = @chan-dequeue-send (ch)
		  (* in *)
		    if NotEqual(item, Q_NIL)
		      then
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
					    self, SELECT(SENDQ_VPROC, item),
					    SELECT(SENDQ_FLS, item),
					    SELECT(SENDQ_CONT, item))
					do SchedulerAction.@atomic-end (self)
					(* in *)
					  return (SELECT(SENDQ_MSG, item))
				    | PEvt.CLAIMED => (* may be claimed, so spin *)
					do Pause()
					apply matchLp()
				    | PEvt.SYNCHED => (* some other thread got it *)
					apply tryLp()
				  end
			    (* in *)
			      apply matchLp ()
		      else
			cont recvK (x : any) = return (x)
			(* in *)
			  let fls : FLS.fls = FLS.@get-in-atomic(self)
			  let flg : PEvt.event_state = alloc(PEvt.WAITING)
			  do @chan-enqueue-recv (ch, flg, self, fls, recvK)
			  SPIN_UNLOCK(ch, CH_LOCK)
			  (* in *)
			    SchedulerAction.@stop-from-atomic(self)
	    (* in *)
	      apply tryLp ()
	  ;
    
	define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
	    let ch : chan_rep = #0(arg)
	    let msg : any = #1(arg)
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    SPIN_LOCK(ch, CH_LOCK)
	    fun tryLp () : unit =
		  let item : recvq_item = @chan-dequeue-recv(ch)
		  (* in *)
		    if NotEqual(item, Q_NIL)
		      then
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
					if Equal(self, SELECT(RECVQ_VPROC, item))
					  then (* sending to a local thread *)
					    cont sendK (_ : unit) = return (UNIT)
					    (* in *)
					      let fls : FLS.fls = FLS.@get-in-atomic(self)
					      do VProcQueue.@enqueue-in-atomic (self, fls, sendK)
					      do FLS.@set-in-atomic(self, SELECT(RECVQ_FLS, item))
					      do SchedulerAction.@atomic-end (self)
					      let k : cont(any) = SELECT(RECVQ_CONT, item)
					      (* in *)
						throw k (msg)
					  else (* sending to a remote thread *)
					    do SchedulerAction.@atomic-end (self)
					    let k : cont(any) = SELECT(RECVQ_CONT, item)
					    cont recvk (_ : unit) = throw k (msg)
					    (* in *)
					      do VProcQueue.@enqueue-on-vproc (
						    SELECT(RECVQ_VPROC, item), SELECT(RECVQ_FLS, item),
						    recvk)
					      return (UNIT)
				    | PEvt.CLAIMED => (* may be claimed, so spin *)
					do Pause()
					apply matchLp()
				    | PEvt.SYNCHED => (* some other thread got it *)
					apply tryLp()
				  end
			    (* in *)
			      apply matchLp ()
			else
			  cont sendK (_ : unit) = return (UNIT)
			  (* in *)
			    let fls : FLS.fls = FLS.@get-in-atomic(self)
			    let flg : PEvt.event_state = alloc(PEvt.WAITING)
			    do @chan-enqueue-send (ch, flg, msg, self, fls, sendK)
			    SPIN_UNLOCK(ch, CH_LOCK)
			    (* in *)
			      SchedulerAction.@stop-from-atomic(self)
	      (* in *)
		apply tryLp ()
	;

      (***** Event constructors *****)
	
	define @chan-recv-evt (ch : chan_rep / exh : exh) : PEvt.pevent =
	    fun pollFn () : bool = @chan-waiting-send(ch)
	    fun doFn (self : vproc, recvK : cont(any) / _ : exh) : () =
		  SPIN_LOCK(ch, CH_LOCK)
		  fun tryLp () : () =
			let item : sendq_item = @chan-dequeue-send (ch)
			(* in *)
			  if Equal(item, Q_NIL)
			    then
			      SPIN_UNLOCK(ch, CH_LOCK)
			      return ()
			    else
			      let state : PEvt.event_state = #0(item)
			      if Equal(DEREF(state), PEvt.SYNCHED)
				then apply tryLp()
				else
				(* there is a matching send, but we must
				 * check to make sure that some other
				 * thread has not already claimed the event.
				 *)
				  fun matchLp () : () =
					let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
					case sts
					 of PEvt.WAITING => (* we matched the send! *)
					      SPIN_UNLOCK(ch, CH_LOCK)
					      do Threads.@enqueue-ready-in-atomic (
						  self, SELECT(SENDQ_VPROC, item),
						  SELECT(SENDQ_FLS, item),
						  SELECT(SENDQ_CONT, item))
					      do SchedulerAction.@atomic-end (self)
					      (* in *)
						throw recvK (SELECT(SENDQ_MSG, item))
					  | PEvt.CLAIMED =>
					      do Pause ()
					      apply matchLp ()
					  | PEvt.SYNCHED => (* someone else got the event *)
					      apply tryLp ()
					end
				  (* in *)
				    apply matchLp ()
		  (* in *)
		    apply tryLp ()
	    fun blkFn (self : vproc, flg : PEvt.event_state, fls : FLS.fls, recvK : cont(any) / _ : exh) : () =
		  SPIN_LOCK(ch, CH_LOCK)
		(* a loop to try to get an item from the sendq *)
		  fun tryLp () : () =
			let item : sendq_item = @chan-dequeue-send-match (flg, ch)
			(* in *)
			  if NotEqual(item, Q_NIL)
			    then
			      let state : PEvt.event_state = #0(item)
			      if Equal(DEREF(state), PEvt.SYNCHED)
				then apply tryLp()
				else
				(* there is a matching send, but we must
				 * check to make sure that some other
				 * thread has not already claimed the event.
				 *)
				  fun matchLp () : () =
					let mySts : PEvt.event_status = CAS(&0(flg), PEvt.WAITING, PEvt.CLAIMED)
					case mySts
					 of PEvt.WAITING => (* try to claim the matching event *)
					      let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
					      case sts
					       of PEvt.WAITING => (* we matched the send! *)
						    SPIN_UNLOCK(ch, CH_LOCK)
						    do Threads.@enqueue-ready-in-atomic (
							self, SELECT(SENDQ_VPROC, item),
							SELECT(SENDQ_FLS, item),
							SELECT(SENDQ_CONT, item))
						    do SchedulerAction.@atomic-end (self)
                                                    let wrapped : ![vproc] = alloc(#2(item))
						    (* in *)
						      throw recvK (wrapped)
						| PEvt.CLAIMED =>
						  (* the owner is attempting to sync, so roll back
						   * and check the matching event again.
						   *)
						    do #0(state) := PEvt.WAITING
						    do Pause()
						    apply matchLp()
						| PEvt.SYNCHED => (* someone else got it, so roll back *)
						    do #0(state) := PEvt.WAITING
						    apply tryLp()
					      end
					  | _ =>
					    (* this event has already been matched, so we restore
					     * the item to the queue and continue.
					     *)
					      do @chan-push-send (ch, item)
					      SPIN_UNLOCK(ch, CH_LOCK)
					      SchedulerAction.@stop-from-atomic (self)
					end
				  (* in *)
				    apply matchLp ()
			    else
			      do @chan-enqueue-recv (ch, flg, self, fls, recvK)
			      SPIN_UNLOCK(ch, CH_LOCK)
			      (* in *)
				return ()
		  (* in *)
		    apply tryLp ()
	  (* in *)
	    return (PEvt.BEVT(pollFn, doFn, blkFn))
	  ;

	define @chan-send-evt (arg : [chan_rep, any] / exh : exh) : PEvt.pevent =
	    let ch : chan_rep = #0(arg)
	    let msg : any = #1(arg)
	    fun pollFn () : bool = @chan-waiting-recv(ch)
	    fun doFn (self : vproc, sendK : cont(unit) / _ : exh) : () =
		  SPIN_LOCK(ch, CH_LOCK)
		  fun tryLp () : () =
			let item : recvq_item = @chan-dequeue-recv (ch)
			(* in *)
			  if Equal(item, Q_NIL)
			    then
			      SPIN_UNLOCK(ch, CH_LOCK)
			      return ()
			    else
			      let state : PEvt.event_state = #0(item)
			      if Equal(DEREF(state), PEvt.SYNCHED)
				then apply tryLp()
				else
				(* there is a matching recv, but we must check to make sure
				 * that some other thread has not already claimed the event.
				 *)
				  fun matchLp () : () =
					let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
					case sts
					 of PEvt.WAITING => (* we matched the recv! *)
					      SPIN_UNLOCK(ch, CH_LOCK)
					      if Equal(self, SELECT(RECVQ_VPROC, item))
						then (* sending to a local thread *)
						  let fls : FLS.fls = FLS.@get-in-atomic(self)
						  do VProcQueue.@enqueue-in-atomic (self, fls, sendK)
						  do FLS.@set-in-atomic(self, SELECT(RECVQ_FLS, item))
						  do SchedulerAction.@atomic-end (self)
						  let k : cont(any) = SELECT(RECVQ_CONT, item)
						  (* in *)
						    throw k (msg)
						else (* sending to a remote thread *)
						  do SchedulerAction.@atomic-end (self)
						  let k : cont(any) = SELECT(RECVQ_CONT, item)
						  cont recvk (_ : unit) = throw k (msg)
						  (* in *)
						    do VProcQueue.@enqueue-on-vproc (
							SELECT(RECVQ_VPROC, item),
							SELECT(RECVQ_FLS, item), recvk)
						    throw sendK (UNIT)
					  | PEvt.CLAIMED =>
					      do Pause ()
					      apply matchLp ()
					  | PEvt.SYNCHED => (* someone else got the event *)
					      apply tryLp ()
					end
				  (* in *)
				    apply matchLp ()
		  (* in *)
		    apply tryLp ()
	    fun blkFn (self : vproc, flg : PEvt.event_state, fls : FLS.fls, sendK : cont(unit) / _ : exh) : () =
		  SPIN_LOCK(ch, CH_LOCK)
		(* a loop to try to get an item from the sendq *)
		  fun tryLp () : () =
			let item : recvq_item = @chan-dequeue-recv-match (flg, ch)
			(* in *)
			  if NotEqual(item, Q_NIL)
			    then
			      let state : PEvt.event_state = #0(item)
			      if Equal(DEREF(state), PEvt.SYNCHED)
				then apply tryLp()
				else
				(* there is a matching recv, but we must check to make sure that
				 * some other thread has not already claimed the event.
				 *)
				  fun matchLp () : () =
					let mySts : PEvt.event_status = CAS(&0(flg), PEvt.WAITING, PEvt.CLAIMED)
					case mySts
					 of PEvt.WAITING => (* try to claim the matching event *)
					      let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
					      case sts
					       of PEvt.WAITING => (* we matched the recv! *)
						    if Equal(self, SELECT(RECVQ_VPROC, item))
						      then (* sending to a local thread *)
							let fls : FLS.fls = FLS.@get-in-atomic(self)
							do VProcQueue.@enqueue-in-atomic (self, fls, sendK)
							do FLS.@set-in-atomic(self, SELECT(RECVQ_FLS, item))
							do SchedulerAction.@atomic-end (self)
							let k : cont(any) = SELECT(RECVQ_CONT, item)
							(* in *)
							  throw k (msg)
						      else (* sending to a remote thread *)
							do SchedulerAction.@atomic-end (self)
							let k : cont(any) = SELECT(RECVQ_CONT, item)
							cont recvk (_ : unit) = throw k (msg)
							(* in *)
							  do VProcQueue.@enqueue-on-vproc (
							      SELECT(RECVQ_VPROC, item),
							      SELECT(RECVQ_FLS, item), recvk)
							  throw sendK (UNIT)
						| PEvt.CLAIMED =>
						  (* the owner is attempting to sync, so roll back
						   * and check the matching event again.
						   *)
						    do #0(state) := PEvt.WAITING
						    do Pause()
						    apply matchLp()
						| PEvt.SYNCHED => (* someone else got it, so roll back *)
						    do #0(state) := PEvt.WAITING
						    apply tryLp()
					      end
					  | _ =>
					    (* this event has already been matched, so we restore
					     * the item to the queue and continue.
					     *)
					      do @chan-push-recv (ch, item)
					      SPIN_UNLOCK(ch, CH_LOCK)
					      SchedulerAction.@stop-from-atomic (self)
					end
				  (* in *)
				    apply matchLp ()
			    else
			      do @chan-enqueue-send (ch, flg, msg, self, fls, sendK)
			      SPIN_UNLOCK(ch, CH_LOCK)
			      (* in *)
				return ()
		  (* in *)
		    apply tryLp ()
	  (* in *)
	    return (PEvt.BEVT(pollFn, doFn, blkFn))
	  ;

      )

    type 'a chan = _prim (chan_rep)

    val new : unit -> 'a chan		= _prim (@chan-new)
    val send : ('a chan * 'a) -> unit	= _prim (@chan-send)
    val recv : 'a chan -> 'a		= _prim (@chan-recv)

    val sendEvt : ('a chan * 'a) -> unit PrimEvent.pevent	= _prim (@chan-send-evt)
    val recvEvt : 'a chan -> 'a PrimEvent.pevent		= _prim (@chan-recv-evt)

  end
