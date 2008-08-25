(* tests.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Stress test the runtime.
 *)

#define ONE_SEC 1000000:long

structure Tests =
  struct

    structure PT = PrimTypes
    structure FLS = FiberLocalStorage

    fun t () = false
    fun failDeadlock () = UnitTesting.validate "deadlock" t
    fun failAns () = UnitTesting.validate "answer" t
(*
  (* check for deadlock *)
    _primcode(
      define @check-for-deadlock(f : fun(PT.unit / PT.exh -> PT.bool), wait : Time.time / exh : PT.exh) : () =
       (* flag for absence of deadlock *)
        let noDeadlock : ![PT.bool] = alloc(FALSE)
        let noDeadlock : ![PT.bool] = promote(noDeadlock)
       (* flag for correct answer *)
        let ans  : ![PT.bool] = alloc (FALSE)
        let ans  : ![PT.bool] = promote(ans)
       (* fiber that performs the check *)
        fun check (_ : PT.unit / exh : PT.exh) : PT.unit =
            let b : PT.bool = apply f(UNIT /exh)
            do UPDATE(0, noDeadlock, TRUE)
	    do UPDATE(0, ans, b)
            return (UNIT)
        let chkK : PT.fiber = Control.@fiber(check / exh)
        let fls : FLS.fls = FLS.@new(UNIT / exh)
       (* start the check in a separate thread *)
        do VProcQueue.@enqueue(fls, chkK / exh)
       (* wait for a random amount of time or until the check finishes *)
        do ThreadOps.@rand-sleep(wait, noDeadlock / exh)
       (* report deadlock *)
        let failDeadlock : fun(PT.unit / PT.exh -> PT.unit) = pmlvar failDeadlock
        do if Equal(noDeadlock, TRUE)
	      then return()
	   else let _ : PT.unit = apply failDeadlock(UNIT / exh)
                return () 
       (* report incorrect answer *)
(* FIXME: enabling this code results in an error in the "uncurry" optimization. *)
(*        let failAns : fun(PT.unit / PT.exh -> PT.unit) = pmlvar failAns
        do if Equal(ans, TRUE)
	      then return()
	   else let _ : PT.unit = apply failAns(UNIT / exh)
                return()
*)
        return()
      ;
    )

*)

  (* control *)
    _primcode(
      define @test1 (x : PT.unit / exh : PT.exh) : PT.unit =
	cont act (s : PT.signal) =
	  case s
	   of STOP => do Control.@forward(STOP / exh)
		      return(UNIT)
	    | PT.PREEMPT (k : PT.fiber) => 
	      do ccall M_Print("Seems to have worked\n")
	      do Control.@forward(STOP / exh)
	      return(UNIT)
	  end
	cont k (x : PT.unit) = do Control.@forward(PT.PREEMPT(k) / exh)
			       return(UNIT)
	do ccall M_Print("Testing run and forward\n")
	do Control.@run(act, k / exh)
	return(UNIT)
      ;

      define @test2 (x : PT.unit / exh : PT.exh) : PT.unit =
	fun lp (i : int / exh : PT.exh) : () =
	    if I32Gte(i, 2)
	       then return()
	    else
		let _ : PT.unit = Control.@yield(/exh)
		apply lp(I32Add(i,1) / exh)
	do apply lp (0 / exh)
	return(UNIT)
      ;

    )
(*
    val _ = Print.printLn "control test: going to sleep"
    val t : unit -> unit = _prim(@test2)
    val _ = t()
    val _ = Print.printLn "control test: waking"
*)
  (* thread operations *)
    _primcode (

      define @ex(x : PT.unit / exh : PT.exh) : PT.unit =
        let flg : ![PT.bool] = alloc(FALSE)
            do ThreadOps.@sleep(I64Mul(I32ToI64(ONE_SEC), 2), flg / exh)
        return(UNIT)
      ;

    )

(*
    val _ = Print.printLn "sleep test: going to sleep"
    val ex : unit -> unit = _prim(@ex)
    val _ = ex()
    val _ = Print.printLn "sleep test: woke up"
*)

  (* locked queues *)
    _primcode(

      define @enq (q : LockedQueue.queue, done : ![PT.bool], wait : PT.bool, i : int / exh : PT.exh) : () = 
	fun enq (i : int / exh : PT.exh) : () =
	    if I32Eq (i,0)
	       then return ()
	       else do if wait 
			  then do ThreadOps.@rand-sleep (ONE_SEC, done / exh)
			       return ()
			  else return ()
		    let wi : PT.ml_int = alloc (i)
		    let wi : any = (any)wi
		    do LockedQueue.@enqueue (q, wi / exh)
		    apply enq (I32Sub(i,1) / exh)
	apply enq (i / exh)
      ;

      define @deq (q : LockedQueue.queue, wait : PT.bool, i : int / exh : PT.exh) : PT.bool =
	fun deq (i : int / exh : PT.exh) : PT.bool =
	    let jOpt : Option.option = LockedQueue.@dequeue (q / exh)
	    case jOpt
	     of NONE =>  (* if deq needs to wait, then yield, otherwise check to see if the count is zero *)
	       let done : PT.bool = I32Eq(i,0)
	       if done 
		  then return (TRUE)
	       else if wait
		  then let _ : PT.unit = Control.@yield ( / exh)
		       apply deq (i / exh)
	       else apply deq (i / exh)
	      | SOME (wj:[int]) => 
			   let j :int = unwrap(wj)
			   if I32Eq (i,j)
			     then let i : int =  I32Sub (i,1)
				  apply deq (i / exh)
			     else return (FALSE)
	    end
	apply deq (i / exh)
      ;

      (* test fifo ordering *)
      define @test-q-1 (/ exh : PT.exh) : PT.bool =
	let nElts : int = 10
	let done :![PT.bool] = alloc (FALSE)
	let done :![PT.bool] = promote(done)
	let q : LockedQueue.queue = LockedQueue.@new (/ exh)
	do @enq (q, done, FALSE, nElts /exh)
	@deq (q, FALSE, nElts / exh)
      ;

      (* test parallel enqueue and dequeue *)
      define @test-q-2 (/ exh : PT.exh) : PT.bool =
	let nElts : int = 50000
	let done :![PT.bool] = alloc (FALSE)
	let done :![PT.bool] = promote(done)
	let q : LockedQueue.queue = LockedQueue.@new (/ exh)
	fun doEnq (x : PT.unit / exh : PT.exh) : PT.unit = 
	    do @enq (q, done, FALSE, nElts / exh)
	    return (UNIT)
      (*  let fls : FLS.fls = Control.@spawn (doEnq / exh) *)
	@deq (q, TRUE, nElts / exh)
      ;

      define @locked-queue-test (x : PT.unit / exh : PT.exh) : PT.unit =
	do_test(test-q-1)
(*        do_concurrent_test(test-q-2, ONE_SEC) *)
(*	
        do_test(test-blocking)
	do_concurrent_test(test-q-4, 20.0:double)
      *)
	return (UNIT)
      ;

    )

    val lockedQueueTest : unit -> unit = _prim(@locked-queue-test)
    val _ = lockedQueueTest()

  (* future1 *)
    fun fib n = if n < 2 then n else fib(n-1) + fib(n-2)
    fun futFib n =
	if n < 2 then n else let
	    fun f () = futFib (n-1)
	    val f1 = Future1.future f
	    in
	      futFib(n-2) + Future1.touch f1
	    end
    fun fut1FibTest n = fib n = futFib n
    fun fut11 () = fut1FibTest 27
    val _ = if fut11()
	    then Print.printLn "fut1 success"
	    else Print.printLn "fut1 fail"


  (* cancelation *)
    _primcode (
      define @test1 (/ exh : PT.exh) : PT.bool =
	cont exit () = return(FALSE)
	let fib : fun(PT.ml_int / PT.exh -> PT.ml_int) = pmlvar fib
	let arg : PT.ml_int = alloc(25)
	let b : ![PT.bool] = alloc(TRUE)
	let b : ![PT.bool] = promote(b)
        let wait : ![PT.bool] = alloc(FALSE)
        let wait : ![PT.bool] = promote(wait)
	fun doit ( x : PT.unit / exh : PT.exh) : PT.unit =
	    do UPDATE(0, wait, TRUE)
	    let x : PT.ml_int = apply fib(arg / exh)
	    do #0(b) := FALSE (* should never get here *)
	    return(UNIT)
	let c : Cancelation.cancelable = Cancelation.@new(UNIT / exh)
	let k : PT.fiber = Control.@fiber(doit / exh)
	let k : PT.fiber = Cancelation.@wrap(c, k / exh)
	let fls : FLS.fls = FLS.@get(/ exh)
	let vps : List.list = ccall ListVProcs(host_vproc)
	do case vps
	 of NIL => throw exit()
	  | List.CONS(vp : vproc, vps : List.list) =>
	    case vps
	     of NIL => throw exit()
	      | List.CONS(vp : vproc, vps : List.list) =>
		VProcQueue.@enqueue-on-vproc(vp, fls, k / exh)
	    end
	end
        fun waitFn () : () = if SELECT(0, wait) then return() else apply waitFn()
        do apply waitFn()
	do Cancelation.@cancel(c / exh)
	let arg : PT.ml_int = alloc(30)
	let x : PT.ml_int = apply fib(arg / exh)
	return(#0(b))
      ;

      define @cancel-test (x : PT.unit / exh : PT.exh) : PT.unit =
        do_test(test1)
        return(UNIT)
      ;
    )
    val cancelTest : unit -> unit = _prim(@cancel-test)
(*    val _ = cancelTest()*)

    structure VPM = VProcMessenger
    _primcode (
      define @messenger-test (x : PT.unit / exh : PT.exh) : PT.unit =
	let vps : List.list = ccall ListVProcs(host_vproc)
	let nth : fun([PT.ml_int, List.list] / PT.exh -> Option.option) = pmlvar List.nth
	let vp2 : Option.option = apply nth(alloc(alloc(3), vps) / exh)
	case vp2
	 of NONE => return(UNIT)
	  | Option.SOME (vp : vproc) =>
	    fun f (x : PT.unit / exh : PT.exh) : any =
		do print_ppt()
		return((any)Option.SOME(alloc(5)))
	    let ch : VPM.chan = VPM.@new(f / exh)
	    do VPM.@send(ch, vp / exh)
	    let opt : Option.option = VPM.@recv(ch / exh)
	    do case opt
		of NONE => 
		   do print_msg("vproc is asleep")
		   return()
		 | SOME (x : PT.ml_int) => return()
	    end
	    return(UNIT)
	end
    ;
    )
    val messengerTest : unit -> unit = _prim(@messenger-test)
    val _ = messengerTest()

  end
