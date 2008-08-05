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

    _primcode (
	typedef chan = ![	    (* all fields are mutable *)
	    bool,			(* spin lock *)
	    list,			(* sendq head *)
	    list,			(* sendq tail *)
	    list,			(* recvq head *)
	    list			(* recvq tail *)
	  ];
    
    define inline @chan-new (_ : unit / exh : exh) : chan =
	let ch : chan = alloc(FALSE, NIL, NIL, NIL, NIL)
	let ch : chan = promote (ch)
	return (ch)
    ;
    
    define inline @chan-acquire-lock (ch : chan / exh : exh) : () =
	fun spin (_ : unit / exh : exh) : () =
	      if CH_TRY_LOCK(ch)
		then apply spin (UNIT / exh)
		else return ()
	(* in *)
	  apply spin (UNIT / exh)
    ;
    
    define @chan-recv (ch : chan / exh : exh) : any =
	let fgs : fgs = @get-fgs(host_vproc / exh)
	do @chan-acquire-lock (ch / exh)
	let maybeItem : option = @chan-dequeue-send (ch / exh)
	(* in *)
	  case maybeItem
	   of SOME(item : sendq_item) =>
		do @chan-release-lock (ch / exh)
		do @atomic-enqueue (#1(item), #2(item) / exh)
		(* in *)
		  return (#0(item))
	    | NONE =>
		cont recvK (x : any) = return (x)
		(* in *)
		  let flag : dirty_flag = alloc(WAITING_EVT)
		  let flag : dirty_flag = promote (flag)
		  do @chan-enqueue-recv (ch, flag, fgs, recvK / exh)
		  do @chan-release-lock (ch / exh)
		  (* in *)
		    @thread-exit(/exh)
	  end
    ;
    
    define @chan-send (arg : [chan, any] / exh : exh) : any =
	let ch : chan = #0(arg)
	let msg : any = #1(arg)
	let fgs : fgs = @get-fgs(host_vproc / exh)
	do @chan-acquire-lock (ch / exh)
	cont sendK (x : unit) = return (x)
	(* in *)
	  fun tryLp (_ : unit / exh : exh) : any =
		let maybeItem : option = @chan-dequeue-recv(ch / exh)
		(* in *)
		  case maybeItem
		   of SOME(item : recvq_item) =>
		      (* there is a matching recv, but we must check to make sure
		       * that some other thread has not already claimed the event.
		       *)
			let success : bool = @event-claim (#0(item) / exh)
			(* in *)
			  if success then (* we got it *)
			    do @chan-release-lock(ch / exh)
			    do @atomic-enqueue (fgs, sendK / exh)
			    (* in *)
			      @throw-to (host_vproc, #1(item), #2(item), msg / exh)
			  else (* someone else got the event, so try again *)
			    apply tryLp (UNIT / exh)
		    | NONE =>
			do @chan-enqueue-send (ch, msg, fgs, sendK / exh)
			do @chan-release-lock (ch / exh)
			(* in *)
			  @thread-exit(/exh)
		  end
	  (* in *)
	    apply tryLp (UNIT / exh)
    ;
    
      )

    type 'a chan = _prim (chan)

    val new : unit -> 'a chan		= _prim (@chan-new)
    val send : ('a chan * 'a) -> unit	= _prim (@chan-send)
    val recv : 'a chan -> 'a		= _prim (@chan-recv)

  end
