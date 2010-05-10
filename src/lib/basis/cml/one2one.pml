(* one2one.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *
 * This is a specialized implementation for point-to-point channels
 *)


structure OneOneChan (*: sig

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


        datatype item_val = SEND of send_val | RECV of recv_val 

    _primcode (

        typedef item_rep = ![
	    item_val
          ];

	typedef chan_rep = ![	    (* all fields are mutable *)
	    any	           	    (* link to item *)
	  ];


#	define ITEM_VPROC	0
#	define ITEM_FLS	        1
#	define ITEM_CONT	2
#       define ITEM_MSG         3

#	define Q_NIL	enum(0) : any


      (***** Channel operations *****)

        define inline @new-send-item (vp : vproc, fls : FLS.fls, k : cont(any), msg : any) : item_rep =
	    let sendval : send_val = alloc(vp, fls, k, msg)
	    let sendval : send_val = promote(sendval)
            let itemval : item_val = SEND (sendval)
            let item : item_rep = alloc(itemval)
            let item : item_rep = promote(item)
              return (item)
	  ;     

        define inline @new-recv-item (vp : vproc, fls : FLS.fls, k : cont(any)) : item_rep = 
	    let recvval : recv_val = alloc(vp, fls, k)
	    let recvval : recv_val = promote(recvval)
            let itemval : item_val = RECV (recvval)
            let item : item_rep = alloc(itemval)
            let item : item_rep = promote(item)
              return (item)
	  ;

	define inline @chan-new (arg : unit / exh : exh) : chan_rep =
	    let ch : chan_rep = alloc(Q_NIL)
	    let ch : chan_rep = promote (ch)
	    return (ch)
	  ;


	define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
	    let ch : chan_rep = #0(arg)
            let msg : any = #1(arg)
	    let self : vproc = SchedulerAction.@atomic-begin ()
	    let chanpt : any = DEREF(ch)
	    cont sendK (_ : unit) = return (UNIT)
            fun commit (itemref : item_rep ) : unit = 
                  let item : item_val = DEREF(itemref)                                                                                                       
                  case item                                                                                                                      
                   of RECV (recvval : recv_val) =>                                                                                                                               
                        do UPDATE(0, ch, Q_NIL)                                                                                                                             
                        if Equal(self, SELECT(ITEM_VPROC, recvval))                                                                                                             
                         then                                                                                                                                                      
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
                  | _ =>                                                                                                                                                          
                      let e : exn = Fail(@"Unexpected waiting processes in channel queue")                                                                                        
                      throw exh (e)                                                          
                end	          
	    if Equal(chanpt, Q_NIL)
	      then
	        let fls : FLS.fls = FLS.@get-in-atomic(self)
	        let item : item_rep = @new-send-item(self, fls, sendK, msg)
	        let itempt : any = CAS(&0(ch), Q_NIL, item)
	        if Equal(itempt, Q_NIL)
	          then
		    SchedulerAction.@stop-from-atomic(self)
		  else
		    let itemref : item_rep = itempt 
		    apply commit (itemref) 
	      else
	        let itemref : item_rep = chanpt
	        apply commit (itemref) 
            ;
 
        define @chan-recv (ch : chan_rep / exh : exh) : any =
            let self : vproc = SchedulerAction.@atomic-begin ()
	    let chanpt : any = DEREF(ch)
	    fun commit (itemref : item_rep) : any = 
	          let item : item_val = DEREF(itemref)
		  case item
		   of SEND (sendval : send_val) =>
		        do UPDATE(0, ch, Q_NIL)
                        do Threads.@enqueue-ready-in-atomic (
			       self, SELECT(ITEM_VPROC, sendval),
			       SELECT(ITEM_FLS, sendval),
			       SELECT(ITEM_CONT, sendval))
			do SchedulerAction.@atomic-end(self)
			  return (SELECT(ITEM_MSG, sendval))
		    | _ =>
		        let e : exn = Fail(@"Unexpected waiting processes in channel queue")
			throw exh (e)
	          end		
	    if Equal(chanpt, Q_NIL) 
	      then
	        cont recvK (x: any ) = return (x)
                  let fls : FLS.fls = FLS.@get-in-atomic(self)
                  let item : item_rep = @new-recv-item(self, fls, recvK)
	          let itempt : any = CAS(&0(ch), Q_NIL, item)
	          if Equal(itempt, Q_NIL)
	           then
                     SchedulerAction.@stop-from-atomic(self)
                   else  
		     let itemref : item_rep = itempt 
		     apply commit (itemref) 
	      else	     
	        let itemref : item_rep = chanpt
	        apply commit (itemref)  
          ;

	
  )

    type 'a chan = _prim (chan_rep)

    val new : unit -> 'a chan		= _prim (@chan-new)
    val send : ('a chan * 'a) -> unit	= _prim (@chan-send)
    val recv : 'a chan -> 'a		= _prim (@chan-recv)


  end
