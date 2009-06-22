(* locked-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Concurrent queue structure. 
 *   - the queue structure is modified in place
 *   - the queue structure is protected by spin locks
 *)

structure LockedQueue :
  sig

(*
    _prim(
      define @new () : queue;

      (* NOTE: signals must be masked *)
      define @enqueue-from-atomic (q : queue, elt : queue_item) : ();

      (* NOTE: signals must be masked *)
      define @dequeue-from-atomic (q : queue) : Option.option;
    )
*)

    type 'a queue

    val new : unit -> 'a queue

    val enqueue : 'a queue * 'a -> unit

    val dequeue : 'a queue -> 'a Option.option

  end = struct

#include "../include/spin-lock.def"

#define ELT_HD_OFF         0
#define ELT_TL_OFF         1
#define EMPTY	           enum(0)
#define LOCK_OFF           0
#define HD_OFF             1
#define TL_OFF             2

    structure PT = PrimTypes

    _primcode (

      typedef queue_item = any;

    (* linked queue elements *)
      typedef elt = ![
          queue_item,	(* data *)
	  any		(* next element *)
      ];

    (* locked queue structure *)
      typedef queue = ![
          int32,	(* spin lock *)
	  elt,		(* head *)
	  elt		(* tail *)
      ];

      define @new () : queue =
        let lockedQ : queue = alloc (false, EMPTY, EMPTY)
        let lockedQ : queue = promote (lockedQ)
        return (lockedQ)
      ;

      define @new-w (x : unit / exh : exh) : queue =
	@new()
      ;

      define inline @in-place-enqueue (q : queue, qElt : elt) : () =                   
        let qHd : elt = SELECT(HD_OFF, q)
        let qTl : elt = SELECT(TL_OFF, q) 						  
        do if Equal (qTl, EMPTY)                                                                 
              then return ()                                                                                  
           else let qElt : any = (any)qElt      								  
                do UPDATE(ELT_TL_OFF, qTl, qElt) 						  
	        return () 			    							  
        do UPDATE(TL_OFF, q, qElt)       	     	    	  				          
        if Equal (qHd, EMPTY)  								  
	   then (* the queue was empty *) 									  
            do UPDATE(HD_OFF, q, qElt) 								  
            return () 		    									  
	else return () 											  
     ;

    (* NOTE: signals must be masked *)
      define @enqueue-from-atomic (q : queue, elt : queue_item) : () =
        let qElt : elt = alloc(elt, EMPTY)
        let qElt : elt = promote (qElt)  
        SPIN_LOCK(q, LOCK_OFF)
        do @in-place-enqueue(q, qElt)
        SPIN_UNLOCK(q, LOCK_OFF)
        return()
      ;

      define @enqueue-w (arg : [queue, queue_item] / exh : exh) : unit =
	let vp : vproc = SchedulerAction.@atomic-begin()
	do @enqueue-from-atomic(#0(arg), #1(arg))
	do SchedulerAction.@atomic-end(vp)
	return(UNIT)
      ;

      define inline @in-place-dequeue (q : queue) : Option.option = 		  
        let qTl : elt = SELECT(TL_OFF, q)        	 	     	   	     		  
        let qHd : elt = SELECT(HD_OFF, q) 						  
        if Equal (qHd, EMPTY) 		     							  
	  then (* the queue is empty *)
	    return(Option.NONE)
	else (* the queue is nonempty, so take an element off the queue head *)
          let qNext : any = SELECT(ELT_TL_OFF, qHd)
          let qNext : elt = (elt)qNext 					  
          do if Equal (qHd, qTl) 	   								  
	    then (* there is one element on the queue, so clear out the tail *) 			  
	      let emptyQ : elt = (elt)EMPTY 	  
	      do UPDATE(TL_OFF, q, emptyQ) 							  
	      return () 		       								  
	    else return () 										  
          do UPDATE(HD_OFF, q, qNext) 								  
          let elt : any = SELECT(ELT_HD_OFF, qHd) 						  
          return(Option.SOME(elt))
      ;

    (* NOTE: signals must be masked *)
      define @dequeue-from-atomic (q : queue) : Option.option =
        SPIN_LOCK(q, LOCK_OFF)
        let elt : Option.option = @in-place-dequeue (q)
        SPIN_UNLOCK(q, LOCK_OFF)
        return(elt)
      ;

      define @dequeue-w (q : queue / exh : exh) : Option.option =
        let vp : vproc = SchedulerAction.@atomic-begin()
	let elt : Option.option = @dequeue-from-atomic(q)
        do SchedulerAction.@atomic-end(vp)
        return(elt)
      ;

    )

    type 'a queue = _prim(queue)
    val new : unit -> 'a queue = _prim(@new-w)
    val enqueue : 'a queue * 'a -> unit = _prim(@enqueue-w)
    val dequeue : 'a queue -> 'a Option.option = _prim(@dequeue-w)

  end
