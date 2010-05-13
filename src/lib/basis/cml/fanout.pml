(* prim-chan.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *
 * This is a specialized implementation for fan-out channels
 *)


structure FanOutChan (*: sig

    type 'a chan

    val new : unit -> 'a chan

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a


  end*) = struct


    _primcode (

      (* the representation of a CML thread suspended on a channel *)

        typedef base_val = ![
	    int,                        (* 0: val type *)
	    any                         (* 1: link field *)
	  ];  

	typedef send_val = ![
	    int,                        (* 0: val type *)
	    any,                        (* 1: link field *)
	    vproc,			(* 2: vproc affinity *)
	    FLS.fls,			(* 3: FLS of thread *)
	    cont(unit),			(* 4: thread's continuation *)
	    any                         (* 5: message *)
	  ];

        typedef recv_val = ![
	    int,                         (* 0: val type *)
	    any,                         (* 1: link field *)
            vproc,                       (* 2: vproc affinity *)
            FLS.fls,                     (* 3: FLS of thread *)
            cont(any)                    (* 4: thread's continuation *)
          ];


	typedef chan_rep = ![	    (* all fields are mutable *)
	    any,	            (* head item *)
	    any 		    (* tail item *)
	  ];

	(* offsets into the chan_rep object *)
#	define QUEUE_HD	0
#	define QUEUE_TL	1

	(* offsets in item val *)
#	define ITEM_TYPE        0
#	define ITEM_LINK        1
#	define ITEM_VPROC       2 	
#	define ITEM_FLS	        3
#	define ITEM_CONT	4
#       define ITEM_MSG         5


#       define DUMMY_T        0
#       define SEND_T         1
#       define RECV_T         2


#	define Q_NIL	enum(0) : any


      (***** Channel operations *****)

        define inline @new-send-item (vp : vproc, fls : FLS.fls, k : cont(any), msg : any) : send_val =
	    let sendval : send_val = alloc(SEND_T, Q_NIL, vp, fls, k, msg)
	    let sendval : send_val = promote(sendval)
              return (sendval)
	  ;     

        define inline @new-recv-item (vp : vproc, fls : FLS.fls, k : cont(any)) : recv_val = 
	    let recvval : recv_val = alloc(RECV_T, Q_NIL, vp, fls, k)
	    let recvval : recv_val = promote(recvval)
              return (recvval)
	  ;

	define inline @chan-new (arg : unit / exh : exh) : chan_rep =
	    let dummyitem : base_val = alloc(DUMMY_T, Q_NIL)
	    let ch : chan_rep = alloc((any)dummyitem, (any)dummyitem)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	  ;



	define @chan-recv (ch : chan_rep / exh : exh) : any =
	    let self : vproc = SchedulerAction.@atomic-begin ()
            fun tryLp () : any = 							
	          let hdpt : base_val = SELECT(QUEUE_HD, ch) 
                  let tlpt : base_val = SELECT(QUEUE_TL, ch)
                  let tl_nextpt : any = SELECT(ITEM_LINK, tlpt)
                  if Equal(tl_nextpt, Q_NIL)
                       then
                         let valtype : int = SELECT(ITEM_TYPE, tlpt)
                           case valtype
                            of SEND_T =>  
                                 if Equal(hdpt, tlpt)
                                   then
                                     cont recvK (x : any) = return (x)
                                     (* in *)
                                       let fls : FLS.fls = FLS.@get-in-atomic(self)
                                       let item : recv_val = @new-recv-item(self, fls, recvK)
                                       if Equal(CAS(&ITEM_LINK(tlpt), Q_NIL, item), Q_NIL)
                                         then 
					   SchedulerAction.@stop-from-atomic(self)
                                         else
                                           apply tryLp ()
                                   else
                                     if Equal(CAS(&QUEUE_HD(ch), (any)hdpt, tlpt), hdpt)
                                       then 
				         let sendval : send_val = (send_val) tlpt
					 do Threads.@enqueue-ready-in-atomic (
                                                self, SELECT(ITEM_VPROC, sendval),
						SELECT(ITEM_FLS, sendval),
						SELECT(ITEM_CONT, sendval))
		                         do SchedulerAction.@atomic-end (self)
                                           return (SELECT(ITEM_MSG, sendval))
                                       else
					 apply tryLp () 
                              | _ => 
                                  cont recvK (x : any) = return (x)
                                  (* in *)
                                    let fls : FLS.fls = FLS.@get-in-atomic(self)
                                    let item : recv_val = @new-recv-item(self, fls, recvK) 
                                    if Equal(CAS(&ITEM_LINK(tlpt), Q_NIL, item), Q_NIL)
                                      then 
					SchedulerAction.@stop-from-atomic(self)
				      else
					apply tryLp () 
                            end
                       else 
                         let _ : any = CAS(&QUEUE_TL(ch), (any)tlpt, tl_nextpt)
                           apply tryLp () 
            (* in *) 
              apply tryLp ()
            ;

 
        define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
            let ch : chan_rep = #0(arg)
            let msg : any = #1(arg)
            let self : vproc = SchedulerAction.@atomic-begin ()
            fun tryLp () : unit = 
                  let hdpt : base_val = SELECT(QUEUE_HD, ch)
                  let tlpt : base_val = SELECT(QUEUE_TL, ch)
                  let hd_nextpt : any = SELECT(ITEM_LINK, hdpt)
                    if Equal(hd_nextpt, Q_NIL)
                      then 
                        cont sendK (_ : unit) = return (UNIT)
                        (* in *)
                          let fls : FLS.fls = FLS.@get-in-atomic(self)
                          let item : send_val = @new-send-item(self, fls, sendK, msg)
                          if Equal(CAS(&ITEM_LINK(hdpt), Q_NIL, item), Q_NIL)
                            then 
                              SchedulerAction.@stop-from-atomic(self)
                            else 
			      apply tryLp ()
                      else  
		        let hdnextpt : base_val = hd_nextpt
		        let hd_next_ty : int = SELECT(ITEM_TYPE, hdnextpt) 
                        case hd_next_ty 
                         of RECV_T  => 
			      let recvval : recv_val = (recv_val)hdnextpt 
                              do UPDATE(QUEUE_HD, ch, (any)hdnextpt) 
                              if Equal(self, SELECT(ITEM_VPROC, recvval))
                                then
                                  cont sendK (_ : unit) = return (UNIT)
                                  (* in *)
                                    let fls : FLS.fls = FLS.@get-in-atomic(self)
                                    do VProcQueue.@enqueue-from-atomic (self, fls, sendK)
                                    do FLS.@set-in-atomic(self, SELECT(ITEM_FLS, recvval))
				    do SchedulerAction.@atomic-end (self)
                                    let k : cont(any) = SELECT(ITEM_CONT, recvval)
                                    (* in *)
                                      throw k (msg) 
                                else 
                                  do SchedulerAction.@atomic-end (self)
                                  let k : cont(any) = SELECT(ITEM_CONT, recvval)
                                  cont recvk (_ : unit) = throw k (msg)
                                  (* in *)
                                    do VProcQueue.@enqueue-on-vproc (
                                           SELECT(ITEM_VPROC, recvval), SELECT(ITEM_FLS, recvval),
				           recvk)
                                    return (UNIT) 
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
