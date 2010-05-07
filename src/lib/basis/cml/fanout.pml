(* prim-chan.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *
 * This is a specialized implementation for fan-out channels
 *)

#include "spin-lock.def"

structure FanOutChan (*: sig

    type 'a chan

    val new : unit -> 'a chan

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a


  end*) = struct


    _primcode (

      (* the representation of a CML thread suspended on a channel *)

	typedef send_item = ![
	    any,			(* 0: link field *)
	    vproc,			(* 1: vproc affinity *)
	    FLS.fls,			(* 2: FLS of thread *)
	    cont(unit),			(* 3: thread's continuation *)
	    any                         (* 4: message *)
	  ];

        typedef recv_item = ![
	    any,                         (* 0: link field *)
            vproc,                       (* 1: vproc affinity *)
            FLS.fls,                     (* 2: FLS of thread *)
            cont(any)                    (* 3: thread's continuation *)
          ];

        typedef dummy_item = ![
            any                        (* 0: link field *)
          ];  
  )
        
	type send_item = _prim(send_item)
	type recv_item = _prim(recv_item)
	type dummy_item = _prim(dummy_item)

        datatype queue_item = SEND of send_item | RECV of recv_item | DUMMY of dummy_item

    _primcode (


	typedef chan_rep = ![	    (* all fields are mutable *)
	    queue_item,	            (* head item *)
	    queue_item 		    (* tail item *)
	  ];

	(* offsets into the chan_rep object *)
#	define QUEUE_HD	0
#	define QUEUE_TL	1

	(* offsets in queue items *)
#	define ITEM_LINK	0
#	define ITEM_VPROC	1
#	define ITEM_FLS	        2
#	define ITEM_CONT	3
#       define ITEM_MSG         4

#	define Q_NIL	enum(0) : any


      (***** Channel operations *****)
	
	define inline @chan-new (arg : unit / exh : exh) : chan_rep =
            let dummy_next : any = alloc (Q_NIL)
            let dummy_next : any = promote (dummy_next)
	    let dummy : dummy_item = alloc(dummy_next)
	    let dummy : dummy_item = promote(dummy)
            let dummyitem : queue_item = DUMMY(dummy)
	    let ch : chan_rep = alloc(dummyitem, dummyitem)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	  ;



	define @chan-recv (ch : chan_rep / exh : exh) : any =
	    let self : vproc = SchedulerAction.@atomic-begin ()
          (* a loop *)
            fun tryLp () : any = 							
	          let hd : queue_item = SELECT(QUEUE_HD, ch) 
                  let tl : queue_item = SELECT(QUEUE_TL, ch)
                  (* in *) 
                    case tl
                     of DUMMY (dummy : dummy_item) =>  
		          let tl_nextpt : any = SELECT(ITEM_LINK, dummy)
                          (* in *)
                            if Equal(tl_nextpt, Q_NIL)
	         	      then
                 		cont recvK (x : any) = return (x)
			        (* in *)
			          let fls : FLS.fls = FLS.@get-in-atomic(self)
                                  let item : recv_item = alloc (Q_NIL, fls, self, recvK)
				  let item : recv_item = promote (item)
				  let recvitem : queue_item = RECV(item)
                                  if Equal(CAS(&0(tl_nextpt), Q_NIL, recvitem), Q_NIL) 
                                    then
                                      SchedulerAction.@stop-from-atomic(self)
                                    else 
				      apply tryLp ()
                              else
			        let tl_next : queue_item = DEREF (tl_nextpt)
                                do CAS(&1(ch), tl, tl_next)
                                apply tryLp ()
                      | SEND(senditem : send_item) => 
                          let tl_nextpt : any = SELECT(ITEM_LINK, senditem)
                          (* in *)
                            if Equal(tl_nextpt, Q_NIL)
                              then
                                if Equal(hd, tl)
			          then
                 		    cont recvK (x : any) = return (x)
			            (* in *)
			              let fls : FLS.fls = FLS.@get-in-atomic(self)
                                      let item : recv_item = alloc (Q_NIL, fls, self, recvK)
		                      let item : recv_item = promote (item)
				      let newitem : queue_item = RECV(item)
                                      if Equal(CAS(&0(tl_nextpt), Q_NIL, newitem), Q_NIL)
                                        then
                                          SchedulerAction.@stop-from-atomic(self)
                                        else apply tryLp ()
                                  else
                                    if Equal(CAS(&QUEUE_HD(ch), hd, tl), hd) 
                                      then                                     
                                        do Threads.@enqueue-ready-in-atomic (
                                            self, SELECT(ITEM_VPROC, senditem),
                                            SELECT(ITEM_FLS, senditem),
                                            SELECT(ITEM_CONT, senditem))
                                        do SchedulerAction.@atomic-end (self)
                                          return (SELECT(ITEM_MSG, senditem))
                                      else 
				        apply tryLp () 
                              else
			        let tl_next : queue_item = DEREF (tl_nextpt)
                                do CAS(&QUEUE_TL(ch), tl, tl_next)
                                apply tryLp ()
                      | RECV(recvitem : recv_item) =>
                          let tl_nextpt : any = SELECT(ITEM_LINK, recvitem)
                          (* in *)
                            if Equal(tl_nextpt, Q_NIL)
                              then
                 		cont recvK (x : any) = return (x)
			        (* in *)
			          let fls : FLS.fls = FLS.@get-in-atomic(self)
                                  let item : recv_item = alloc (Q_NIL, fls, self, recvK)
				  let item : recv_item = promote (item)
				  let newitem : queue_item = RECV(item)
                                  if Equal(CAS(&0(tl_nextpt), Q_NIL, newitem), Q_NIL) 
                                    then
                                      SchedulerAction.@stop-from-atomic(self)
                                    else 
				      apply tryLp ()                                
                              else
			        let tl_next : queue_item = DEREF (tl_nextpt)
                                do CAS(&QUEUE_TL(ch), tl, tl_next)
                                apply tryLp ()
                      end      
      (* in *)
              apply tryLp ()
          ; 
 
        define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
            let ch : chan_rep = #0(arg)
            let msg : any = #1(arg)
            let self : vproc = SchedulerAction.@atomic-begin ()
            fun tryLp () : unit = 
                  let hd : queue_item = SELECT(QUEUE_HD, ch)
                  let tl : queue_item = SELECT(QUEUE_TL, ch)
                  fun getNext (qitem : queue_item) : any = 
                        case qitem
                         of DUMMY (dummy : dummy_item) =>
                              let hd_nextpt : any = SELECT(ITEM_LINK, dummy)
                                return (hd_nextpt)
                          | SEND (senditem : send_item) =>
                              let hd_nextpt : any = SELECT(ITEM_LINK, senditem)
                                return (hd_nextpt)
                          | RECV (recvitem : recv_item) =>  
                              let hd_nextpt : any = SELECT(ITEM_LINK, recvitem)
                                return (hd_nextpt)                   
                        end
                  let hd_nextpt : any = apply getNext (hd)
                  (* in *) 
                    if Equal(hd_nextpt, Q_NIL)
                      then 
                        cont sendK (_ : unit) = return (UNIT)
                        (* in *)
                          let fls : FLS.fls = FLS.@get-in-atomic(self)
                          let item : send_item = alloc (Q_NIL, fls, self, sendK, msg)
                          let item : send_item = promote (item)
                          let senditem : queue_item = SEND(item)
                          if Equal(CAS(&0(hd_nextpt), Q_NIL, senditem), Q_NIL)
                            then 
                              SchedulerAction.@stop-from-atomic(self)
                            else 
			      apply tryLp ()
                      else 
		        let hd_next : queue_item = DEREF (hd_nextpt) 
                        case hd_next 
                         of RECV (recvitem : recv_item) => 
                              do UPDATE(QUEUE_HD, ch, hd_next) 
                              if Equal(self, SELECT(ITEM_VPROC, recvitem))
                                then
                                  cont sendK (_ : unit) = return (UNIT)
                                  (* in *)
                                    let fls : FLS.fls = FLS.@get-in-atomic(self)
                                    do VProcQueue.@enqueue-from-atomic (self, fls, sendK)
                                    do FLS.@set-in-atomic(self, SELECT(ITEM_FLS, recvitem))
				    do SchedulerAction.@atomic-end (self)
                                    let k : cont(any) = SELECT(ITEM_CONT, recvitem)
                                    (* in *)
                                      throw k (msg) 
                                else 
                                  do SchedulerAction.@atomic-end (self)
                                  let k : cont(any) = SELECT(ITEM_CONT, recvitem)
                                  cont recvk (_ : unit) = throw k (msg)
                                  (* in *)
                                    do VProcQueue.@enqueue-on-vproc (
                                           SELECT(ITEM_VPROC, recvitem), SELECT(ITEM_FLS, recvitem),
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
