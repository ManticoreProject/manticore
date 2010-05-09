(* fanin.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *
 * This is a specialized implementation for fan-in channels
 *)


structure FanInChan (*: sig

    type 'a chan

    val new : unit -> 'a chan

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a


  end*) = struct


    _primcode (

      (* the representation of a CML thread suspended on a channel *)

	typedef send_val = ![
	    vproc,			(* 0: vproc affinity *)
	    FLS.fls,			(* 1: FLS of thread *)
	    cont(unit),			(* 2: thread's continuation *)
	    any                         (* 3: message *)
	  ];

        typedef recv_val = ![
            vproc,                       (* 0: vproc affinity *)
            FLS.fls,                     (* 1: FLS of thread *)
            cont(any)                    (* 2: thread's continuation *)
          ];

  )
        
	type send_val = _prim(send_val)
	type recv_val = _prim(recv_val)


        datatype item_val = SEND of send_val | RECV of recv_val | DUMMY

    _primcode (

        typedef queue_item = ![
            item_val,                    (* item value *) 
            any                          (* link field *)
          ]; 

	typedef chan_rep = ![	    (* all fields are mutable *)
	    queue_item,	            (* head item *)
	    queue_item 		    (* tail item *)
	  ];

	(* offsets into the chan_rep object *)
#	define QUEUE_HD	0
#	define QUEUE_TL	1

        (* offsets in queue items *)
#       define ITEM_VALUE       0
#       define ITEM_LINK        1

	(* offsets in item valu *)
#	define ITEM_VPROC	0
#	define ITEM_FLS	        1
#	define ITEM_CONT	2
#       define ITEM_MSG         3

#	define Q_NIL	enum(0) : any


      (***** Channel operations *****)

        define inline @new-send-item (vp : vproc, fls : FLS.fls, k : cont(any), msg : any) : queue_item =
	    let sendval : send_val = alloc(vp, fls, k, msg)
	    let sendval : send_val = promote(sendval)
            let itemval : item_val = SEND (sendval)
            let item : queue_item = alloc(itemval, Q_NIL)
            let item : queue_item = promote(item)
              return (item)
	  ;     

        define inline @new-recv-item (vp : vproc, fls : FLS.fls, k : cont(any)) : queue_item = 
	    let recvval : recv_val = alloc(vp, fls, k)
	    let recvval : recv_val = promote(recvval)
            let itemval : item_val = RECV (recvval)
            let item : queue_item = alloc(itemval, Q_NIL)
            let item : queue_item = promote(item)
              return (item)
	  ;

	define inline @chan-new (arg : unit / exh : exh) : chan_rep =
	    let dummyitem : queue_item = alloc(DUMMY, Q_NIL)
	    let dummyitem : queue_item = promote(dummyitem)
	    let ch : chan_rep = alloc(dummyitem, dummyitem)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	  ;



	define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
	    let ch : chan_rep = #0(arg)
            let msg : any = #1(arg)
	    let self : vproc = SchedulerAction.@atomic-begin ()
            fun tryLp () : any = 							
	          let hdpt : queue_item = SELECT(QUEUE_HD, ch) 
                  let tlpt : queue_item = SELECT(QUEUE_TL, ch)
                  let tl_nextpt : any = SELECT(ITEM_LINK, tlpt)
                  if Equal(tl_nextpt, Q_NIL)
                       then
                         let tlval : item_val = SELECT(ITEM_VALUE, tlpt)  
                           case tlval
                            of RECV(recvval : recv_val) =>  
                                 if Equal(hdpt, tlpt)
                                   then
                                     cont sendK (_ : unit) = return (UNIT)
                                     (* in *)
                                       let fls : FLS.fls = FLS.@get-in-atomic(self)
                                       let item : queue_item = @new-send-item(self, fls, sendK, msg)
                                       if Equal(CAS(&ITEM_LINK(tlpt), Q_NIL, item), Q_NIL)
                                         then 
					   SchedulerAction.@stop-from-atomic(self)
                                         else
                                           apply tryLp ()
                                   else
                                     if Equal(CAS(&QUEUE_HD(ch), hdpt, tlpt), hdpt)
                                       then 
					 if Equal(self, SELECT(ITEM_VPROC, recvval))
					   then
					     cont sendK (_ : unit) = return (UNIT)
					     (* in *)
					       let fls : FLS.fls = FLS.@get-in-atomic(self)
					       do VProcQueue.@enqueue-from-atomic(self, fls, sendK)
					       do FLS.@set-in-atomic(self, SELECT(ITEM_FLS, recvval))
                                               do SchedulerAction.@atomic-end(self)
                                               let k : cont(any) = SELECT(ITEM_CONT, recvval)
                                               (* in *)
                                                 throw k (msg)
                                           else
                                             do SchedulerAction.@atomic-end (self) 
                                             let k : cont(any) = SELECT(ITEM_CONT, recvval)
                                             cont recvk (_ : unit) = throw k (msg)
                                             (* in *)
                                               do VProcQueue.@enqueue-on-vproc (
                                                      SELECT(ITEM_VPROC, recvval),
						      SELECT(ITEM_FLS, recvval),
						      recvk)
					       return (UNIT)	  
				       else
				         apply tryLp ()
                              | _ => 
                                  cont sendK (_ : unit) = return (UNIT)
                                  (* in *)
                                    let fls : FLS.fls = FLS.@get-in-atomic(self)
                                    let item : queue_item = @new-send-item(self, fls, sendK, msg) 
                                    if Equal(CAS(&ITEM_LINK(tlpt), Q_NIL, item), Q_NIL)
                                      then 
					SchedulerAction.@stop-from-atomic(self)
				      else
					apply tryLp () 
                            end 
                       else 
		         let tlnextpt : ![item_val, any] =tl_nextpt
                         let _ : any = CAS(&QUEUE_TL(ch), tlpt, tl_nextpt)
                           apply tryLp () 
            (* in *) 
              apply tryLp ()
            ;

 
        define @chan-recv (ch : chan_rep / exh : exh) : any =
            let self : vproc = SchedulerAction.@atomic-begin ()
            fun tryLp () : unit = 
                  let hdpt : queue_item = SELECT(QUEUE_HD, ch)
                  let tlpt : queue_item = SELECT(QUEUE_TL, ch)
                  let hd_nextpt : any = SELECT(ITEM_LINK, hdpt)
                    if Equal(hd_nextpt, Q_NIL)
                      then 
                        cont recvK (x : any) = return (x)
                        (* in *)
                          let fls : FLS.fls = FLS.@get-in-atomic(self)
                          let item : queue_item = @new-recv-item(self, fls, recvK)
                          if Equal(CAS(&ITEM_LINK(hdpt), Q_NIL, item), Q_NIL)
                            then 
                              SchedulerAction.@stop-from-atomic(self)
                            else 
			      apply tryLp ()
                      else  
		        let hdnextpt : ![item_val, any] = hd_nextpt
		        let hd_next_val : item_val = SELECT(ITEM_VALUE, hdnextpt) 
                        case hd_next_val 
                         of SEND (sendval : send_val) => 
                              do UPDATE(QUEUE_HD, ch, hdnextpt) 
                              do Threads.@enqueue-ready-in-atomic (
				     self, SELECT(ITEM_VPROC, sendval),
				     SELECT(ITEM_FLS, sendval),
				     SELECT(ITEM_CONT, sendval))
			      do SchedulerAction.@atomic-end (self)
                              return (SELECT(ITEM_MSG, sendval))
                          | _ => 
			      let e : exn = Fail(@"Unexpected waiting processes in channel queue")
                              throw exh (e)

			end  
            (* in *)
              apply tryLp ()
          ;

	
  )

    type 'a chan = _prim (chan_rep)

    val new : unit -> 'a chan		= _prim (@chan-new)
    val send : ('a chan * 'a) -> unit	= _prim (@chan-send)
    val recv : 'a chan -> 'a		= _prim (@chan-recv)


  end
