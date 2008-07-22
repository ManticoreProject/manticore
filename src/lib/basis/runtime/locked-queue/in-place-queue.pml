(* in-place-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * In place queue operations.
 *)

structure InPlaceQueue =
  struct

    structure PT = PrimTypes

    _primcode (
      typedef elt = ![any, any];

      define @enqueue (q : locked_queue, qElt : elt / exh : PT.exh) : () =                   
        let qHd : elt = SELECT(HD_OFF, q) 						  
        let qTl : elt = SELECT(TL_OFF, q) 						  
        do if Equal (qTl, IN_PLACE_QUEUE_EMPTY)                                                                 
              then return ()                                                                                  
           else let qElt : any = (any)qElt      								  
                 do UPDATE(ELT_TL, qTl, qElt) 						  
	         return () 			    							  
        do UPDATE(TL_OFF, q, qElt)       	     	    	  				          
        if Equal (qHd, IN_PLACE_QUEUE_EMPTY)  								  
	   then (* the queue was empty *) 									  
            do UPDATE(HD_OFF, q, qElt) 								  
            return () 		    									  
	else return () 											  
      ;    	  	 											  
													  
      define @dequeue (q : locked_queue  / exh : PT.exh) : Option.option = 		  
        let qTl : elt = SELECT(TL_OFF, q)        	 	     	   	     		  
        let qHd : elt = SELECT(HD_OFF, q) 						  
        if Equal (qHd, IN_PLACE_QUEUE_EMPTY) 		     							  
           then (* the queue is empty *) 									  
                return(NONE)
	else (* the queue is nonempty, so take an element off the queue head *) 				  
          let qNext : any = SELECT(ELT_TL, qHd) 	     	  				  
          let qNext : elt = (elt)qNext 					  
          do if Equal (qHd, qTl) 	   								  
                then (* there is one element on the queue, so clear out the tail *) 			  
                     let emptyQ : elt = (elt)IN_PLACE_QUEUE_EMPTY 	  
                     do UPDATE(TL_OFF, q, emptyQ) 							  
                     return () 		       								  
                else return () 										  
          do UPDATE(HD_OFF, q, qNext) 								  
          let elt : any = SELECT(ELT_HD, qHd) 						  
          return(Option.SOME(elt))
      ;

    )

  end
