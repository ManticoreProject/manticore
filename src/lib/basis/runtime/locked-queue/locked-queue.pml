(* locked-queue.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Non-blocking locked queue.
 *)

structure LockedQueue =
  struct

    structure PT = PrimTypes

    _primcode (
      typedef queue_elt = any;
      typedef in_place_queue_elt = InPlaceQueue.elt;
    (* lock, head, tail, blocked fiber head, blocked fiber tail *)
      typedef locked_queue = ![PT.bool, in_place_queue_elt, in_place_queue_elt, in_place_queue_elt, in_place_queue_elt];
      
      define @dequeue (q : locked_queue / exh : PT.exh) : Option.option =
        let mask : bool = @spin-lock-lq (q / exh)
        let elt : option = hlop InPlaceQueue.@dequeue (q / exh)
        do hlop SpinLock.@spin-unlock(q, mask)
      ;

      define @enqueue (q : locked_queue, elt : any / exh : PT.exh) : () =
       (* promote elt *before* acquiring the lock *)
        let qElt : in_place_queue_elt = LOCKED_QUEUE_NEW_ELT(elt)
        let qElt : in_place_queue_elt = promote (qElt)  
        let mask : bool = hlop SpinLock.@spin-lock (q / exh)
       (* check for blocked threads first *)
        let bqHd : in_place_queue_elt = SELECT(LOCKED_QUEUE_BLOCKED_HD, q)
        do if Equal (bqHd, NIL)
              then (* nothing is blocked; enqueue the element *)
                 InPlaceQueue.@enqueue (q, qElt / exh)
           else (* unblock the thread and pass it the element *)
                let elt : Option.option = InPlaceQueue.@dequeue (q / exh)
                case elt
		 of NONE => 
		    let _ : unit = @spin-unlock-lq (q, mask / exh)
(* fixme: raise an error *)
                    return()
		  | SOME (k : Control.fiber) =>
                    do hlop SpinLock.@unlock (q, mask / exh)
                    let blockedThread : locked_queue_blocked_thread = (locked_queue_blocked_thread)blockedThread
                    let blockedK : cont(any) = SELECT(LOCKED_QUEUE_BLOCKED_THREAD_CONT, blockedThread)
                    let blockedFgs : FLS.fls = SELECT(LOCKED_QUEUE_BLOCKED_THREAD_FGS, blockedThread)
                    let vp : vproc = SELECT(LOCKED_QUEUE_BLOCKED_THREAD_VPROC, blockedThread)
                    cont unblockK (x : unit) =
                         throw blockedK (elt)
                   let unblockK : Control.fiber = (Control.fiber)unblockK
                   VProc.@enqueue-on (vp, blockedFgs, unblockK / exh)
                   do hlop SpinLock.@unlock (q, mask / exh)
                   return ()
      ;

      define @new ( / exh : PT.exh) : locked_queue =
        let lockedQ : locked_queue = alloc (FALSE, IN_PLACE_QUEUE_EMPTY, IN_PLACE_QUEUE_EMPTY, IN_PLACE_QUEUE_EMPTY, IN_PLACE_QUEUE_EMPTY)
        let lockedQ : locked_queue = promote (lockedQ)
        return (lockedQ)


    )

  end
