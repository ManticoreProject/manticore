signature FUTURE =
  sig
      type 'a future

    (* create a future *)
      val future : (unit -> 'a) -> 'a future
    (* touch a future *)
      val touch : 'a future -> 'a

  end

structure Future1 :> FUTURE = 
  struct

    type 'a thunk = unit -> 'a

  (* a future1 cell consists of two words:
   *     1) a _state_ word, with one of the following values:
   *          emptyF
   *          stolenF
   *          evalF
   *          full      value
   *          waiting   cont
   *     2) a _thunk_ word 
   *     3) a cancel cell for cancelling the future's evaluation
   *     4) fiber-group storage for the future (tracks parent->child relationships)
   *)
    type 'a future = prim (![Option.option('a), thunk('a), Cancelation.cancelable, FLS.fls])
		     
    val emptyF : prim(any) = prim(return($0))
    val stolenF : prim(any) = prim(return($1))
    val evalF : prim(any) = prim(return($2))

  (* scheduling code *)
    type queue = Process.fiber LockedQueue.queue

    val scheduler : prim(fun ( / exh -> queue)) = prim(
                  extern void* ListVProcs(void*)
                  fun scheduler ( / exh : exh) : queue =
                      (* shared queue for the workers *)
                      let q : queue = apply LockedQueue.new ( / exh)
                      (* Initialize fiber-group storage. *)
                      let parentFLS : fls = FLS.get ( / exh)
                      let fls : Fls.fls = parentFLS
                      fun mkSwitch (self : vproc / exh : exh) : sigact = 
                         (* the scheduler action *)
                         cont switch (s : Process.signal) =
                              cont dispatch () = 
                                    let kOpt : Option.option('a) = apply LockedQueue.dequeue (q / exh)
                                    case kOpt 
				     of NONE => 
 		                        let _ : unit = apply Process.atomicYield ( / exh)
		                        throw dispatch()
				      | SOME (k : Process.fiber) =>
                                        throw Process.run (switch, fls, k / exh)
		                    end
                              case s
			       of Process.STOP => throw dispatch ()
				| Process.PREEMPT k =>
                                  do apply LockedQueue.enqueue (q, s / exh)
                                  let _ : unit = apply Process.atomicYield (self / exh)
                                  throw dispatch ()
                        return (switch)
                    (* initialize the scheduler on the vprocs. *)
                    let vps : list = ccall ListVProcs(host_vproc)
                    do Process.schedulerStartup (mkSwitch, parentFLS, vps / exh)
                    return (q)
                 return(scheduler))

    val eval : prim(fun(future('a) / exh -> 'a)) = prim(
                  fun eval (fut : future('a) / exh : exh) : 'a =
                      let f : thunk('a) = SELECT(1, fut)
                      (* clear the thunk pointer to avoid a space leak *)
                      do UPDATE(1, fut, (thunk('a)) $0)
                      let resultLocal : 'a = apply f (UNIT / exh)
                      let result : 'a = promote (resultLocal)
                      return(result)
                  return(eval))

    val steal : prim(fun(queue, future('a) -> ())) = prim(
                  fun steal (futuresQ : queue,  fut : future('a) / exh) : () =
		        let tmp : any = CAS (&0(fut), emptyF, stolenF)
                        if Equal (tmp, emptyF) 
                           then let result : any = apply eval(fut / exh)
				do apply Log.log(Log.RTFuture1StealEvt)
                                let tmpX : any = CAS(&0(fut), stolenF, result)
                                if Equal (tmpX, stolenF)            
                                   then return ()
                                   else (* unblock the future *)
           		                do UPDATE(0, fut, result)
   	                                let k : Process.fiber = (Process.fiber) tmpX
                                        do LockedQueue.enqueue (futuresQ, k / exh)
                                        return ()
                            else (* future cell is already full *)
                                 return ()
                  return(steal))

    val future : (unit -> 'a) -> 'a future = prim(
                  fun future (thunk : thunk('a) / exh : exh) : future('a) =
                      (* get the fiber-group storage of the caller *)
                      let fls : FLS.fls = FLS.get (host_vproc / exh)
                      (* create the cancel cell for the future *)
                      let c : Cancelation.cancelable = apply Cancelation.new(/exh)
                      let fut : future('a) = alloc (emptyF, thunk, c, fls)
                      let fut : future('a) = promote (fut)
                      (* get the futures queue *)
                      let futuresQOpt : Option.option(queue) = FLS.find (tag(future1), initSched, fls / exh)
                      let futuresQ : queue = case futuresQOpt
                          of NONE => let futuresQ : queue = apply scheduler ( / exh)
                                     return (futuresQ)
			   | SOME(futuresQ : queue) => return(futuresQ)
                          end
                      (* create the fiber for evaluating the future *)
                      fun wrapper (_ : unit / exh : exh) : unit =
                          do apply steal (futuresQ, fut / exh)
                          return (UNIT)
                      let k : Process.fiber = apply Process.fiber (wrapper / exh)
                      let k : Process.fiber = Cancelation.wrap(c, k / exh)
                      (* add the future to the scheduling queue *)
                      do apply LockedQueue.enqueue (futuresQ, k / exh)
                      (* log the spawn *)
                      do apply Log.log(Log.RTFuture1SpawnEvt)
                      return (fut) 
                  return(future))

    val touch : 'a future -> 'a = prim(
                fun touch(fut : future('a) / exh : exh) : 'a =
                    let tmp : any = CAS (&0(fut), emptyF, evalF)
                    if Equal (tmp, emptyF)
                       then (* the future is ready for evaluation *)
                            let result : any = apply eval(fut / exh)
                            do Log.log(Log.RTFuture1TouchEvt)
                            do UPDATE(0, fut, result)
                            return (result)
                    else if Equal (tmp, STOLEN_F)
                       then (* someone else is evaluating the future; we need to block *)
                       cont kLocal (_ : unit) = 
                           (* resume the future *)
		           return (SELECT(0, fut))
                       let kLocal : Process.fiber = (Process.fiber)kLocal
                       (* make the future cancelable *)
                       let kLocal : Process.fiber = apply Cancelation.wrap(SELECT(2, fut), kLocal / exh)
                       let k : Process.fiber = promote (kLocal)
   	               let tmpX : any = CAS (&0(fut), stolenF, k)
 	               if Equal (tmpX, stolenF)
	                 then (* transfer control to the scheduler *)
                              throw Process.stop ()
	                 else return (tmpX)
                   else return (tmp)
               return(touch))

  end (* Future1 *)
