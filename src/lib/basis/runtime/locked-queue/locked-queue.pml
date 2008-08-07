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

    _primcode (

    (* items stored in the queue *)
      typedef item = any;

    (* suspended thread, blocked on a dequeue operation *)
      typedef blocked_thread = [
          cont(any),       (* continuation of the blocked thread (expects the element) *)
	  FLS.fls,         (* fiber local storage of the blocked thread *)
	  vproc            (* vproc where the thread blocked *)
      ];

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
		  | Option.SOME (blockedThread : blocked_thread) =>
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

(*
define @random-wait (done : ![bool] /exh: exh) : () =
     (* percentage of the time that we do not wait *)
      let pWait : double = 0.90:double
      let one : ml_double = wrap(1.0:double)
      let zero : ml_double = wrap (0.0:double)
      let arg : [ml_double, ml_double] = alloc(zero, one)
      let pW : [double] = @drand (arg / exh)
      let p : double = unwrap (pW)
      if F64Lt (p, pWait)
         then return ()
         else @wait (0.1:double, done / exh)
;

(* enqueue at randomly chosen times *)
define @enq-at-random (q : queue, done : ![PT.bool], wait : PT.bool, n : int / exh : PT.exh) : () = 
  fun enq (i : int / exh : PT.exh) : () =
      if I32Eq (i,0)
         then return ()
         else do if wait 
		    then do @random-wait (done/exh)
			 return ()
                    else return ()
              let wi : [int] = wrap (i)
              let wi : any = (any)wi
              do @enqueue (q, wi / exh)
              apply enq (I32Sub(i,1) / exh)
  apply enq (n / exh)
;

(* test fifo ordering *)
define @test-q-1 (/ exh : PT.exh) : PT.bool =
  let nElts : int = 1080
  let done :![PT.bool] = alloc (FALSE)
  let done :![PT.bool] = promote(done)
  let q : I.queue = @new (/ exh)
  do @enq-at-random (q, done, FALSE, nElts /exh)
  @deq (q, FALSE, nElts / exh)
;

define @locked-queue-test-startup ( / exh : PT.exh) : () =
  do_test(test-q-1)
(*  do_concurrent_test(test-q-2, 20.0:double)
  do_test(test-q-3)
  do_concurrent_test(test-q-4, 20.0:double)
*)
  return ()
;
*)

    )

  end

(* TODO: testing *)
