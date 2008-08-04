(* locked-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Locked queues using spin locks.
 *)

structure LockedQueue =
  struct

    structure PT = PrimTypes
    structure I = InPlaceQueue
    structure FLS = FiberLocalStorage

    _primcode (

    (* items stored in the queue *)
      typedef item = any;
    
      define @dequeue (q : I.queue / exh : PT.exh) : Option.option =
        let mask : PT.bool = SpinLock.@lock (q / exh)
        let elt : Option.option = I.@dequeue (q / exh)
        do SpinLock.@unlock(q, mask / exh)
        return(elt)
      ;

      define @enqueue (q : I.queue, elt : any / exh : PT.exh) : () =
       (* promote elt *before* acquiring the lock *)
        let qElt : I.elt = NEW_ELT(elt)
        let qElt : I.elt = promote (qElt)  
        let mask : PT.bool = SpinLock.@lock (q / exh)
       (* check for blocked threads first *)
        let bqHd : I.elt = SELECT(BLOCKED_HD_OFF, q)
        do if Equal (bqHd, NIL)
              then (* nothing is blocked; enqueue the element *)
                 I.@enqueue (q, qElt / exh)
           else (* unblock the thread and pass it the element *)
                let elt : Option.option = I.@dequeue (q / exh)
                case elt
		 of NONE => 
		    do SpinLock.@unlock (q, mask / exh)
                    do assert(FALSE)  (* error *)
                    return()
		  | Option.SOME (blockedThread : I.blocked_thread) =>
                    do SpinLock.@unlock (q, mask / exh)
                    let blockedK : cont(any) = SELECT(BLOCKED_THREAD_CONT_OFF, blockedThread)
                    let blockedFgs : FLS.fls = SELECT(BLOCKED_THREAD_FGS_OFF, blockedThread)
                    let vp : vproc = SELECT(BLOCKED_THREAD_VPROC_OFF, blockedThread)
                    cont unblockK (x : PT.unit) =
                         throw blockedK (elt)
                    let unblockK : PT.fiber = (PT.fiber)unblockK
                    do VProcQueue.@enqueue-on-vproc (vp, blockedFgs, unblockK / exh)
                    do SpinLock.@unlock (q, mask / exh)
                    return ()
                end
         return()
      ;

      define @new ( / exh : PT.exh) : I.queue =
        let lockedQ : I.queue = alloc (FALSE, EMPTY, EMPTY, EMPTY, EMPTY)
        let lockedQ : I.queue = promote (lockedQ)
        return (lockedQ)
      ;

    )

  end

(* TODO: testing *)
