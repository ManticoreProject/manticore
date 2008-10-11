(* locked-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Unbounded-length locked queues using spin locks.
 *)

structure LockedQueue =
  struct

    structure PT = PrimTypes
    structure I = InPlaceQueue
    structure FLS = FiberLocalStorage
    structure SpinLock = SPIN_LOCK_NAME

    _primcode (

    (* items stored in the queue *)
      typedef item = any;
      typedef queue = I.queue;

    (* suspended thread, blocked on a dequeue operation *)
      typedef blocked_thread = [
          cont(any),       (* continuation of the blocked thread (expects the element) *)
	  FLS.fls,         (* fiber local storage of the blocked thread *)
	  vproc            (* vproc where the thread blocked *)
      ];

      define @dequeue (q : queue / exh : PT.exh) : Option.option =
        let mask : PT.bool = SpinLock.@lock (q / exh)
        let elt : Option.option = I.@dequeue (q / exh)
        do SpinLock.@unlock(q, mask / exh)
        return(elt)
      ;

      define @enqueue (q : queue, elt : any / exh : PT.exh) : () =
       (* promote elt *before* acquiring the lock *)
        let qElt : I.elt = NEW_ELT(elt)
        let qElt : I.elt = promote (qElt)  
        let mask : PT.bool = SpinLock.@lock (q / exh)
       (* check for blocked threads first *)
        let bqHd : I.elt = SELECT(BLOCKED_HD_OFF, q)
        do if Equal (bqHd, nil)
              then (* nothing is blocked; enqueue the element *)
                 do I.@enqueue (q, qElt / exh)
		 SpinLock.@unlock (q, mask / exh)
           else (* unblock the thread and pass it the element *)
                let elt : Option.option = I.@dequeue (q / exh)
                case elt
		 of Option.NONE => 
		    do SpinLock.@unlock (q, mask / exh)
                    do assert(PT.false)  (* error *)
                    return()
		  | Option.SOME (blockedThread : blocked_thread) =>
                    do SpinLock.@unlock (q, mask / exh)
                    let blockedK : cont(any) = SELECT(BLOCKED_THREAD_CONT_OFF, blockedThread)
                    let blockedFgs : FLS.fls = SELECT(BLOCKED_THREAD_FGS_OFF, blockedThread)
                    let vp : vproc = SELECT(BLOCKED_THREAD_VPROC_OFF, blockedThread)
                    cont unblockK (x : PT.unit) =
                         throw blockedK (elt)
                    let unblockK : PT.fiber = (PT.fiber)unblockK
                    do VProcQueue.@enqueue-on-vproc (vp, blockedFgs, unblockK / exh)
                    SpinLock.@unlock (q, mask / exh)
                end
         return()
      ;

      define @new ( / exh : PT.exh) : queue =
        let lockedQ : queue = alloc (PT.false, EMPTY, EMPTY, EMPTY, EMPTY)
        let lockedQ : queue = promote (lockedQ)
        return (lockedQ)
      ;

    )

  end
