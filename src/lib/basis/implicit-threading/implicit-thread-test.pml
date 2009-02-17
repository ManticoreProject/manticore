structure I = ImplicitThread
structure PT = PrimTypes
_primcode (
(*
define @test (x : unit / exh : exh) : unit =
  fun spawnFn (k : ImplicitThread.thread / exh : exh) : unit =
      return(UNIT)
  cont workerInit (x : unit) =
      do print_ppt()
      SchedulerAction.@stop()
  let group : I.group = I.@group(workerInit, spawnFn / exh)
  return(UNIT)
;
*)
(*
define @test3 (x : unit / exh : exh) : unit =
  let x : ![int] = alloc(0)
  let x : ![int] = promote(x)
  cont k1 (y : unit) =
      do print_ppt()
      if I32Eq(#0(x), 3)
         then 
	    let _ : unit = SchedulerAction.@stop()
	    do print_ppt()
	    let e : exn = Match
	    throw exh(e)
         else 
	     let _ : unit = SchedulerAction.@yield(/ exh)
             throw k1(UNIT)
  cont k2 (y : unit) =
      do print_ppt()
      do UPDATE(0, x, I32Add(#0(x), 1))
      if I32Eq(#0(x), 3)
         then 
	    return(UNIT)
         else 
	     let _ : unit = SchedulerAction.@yield(/ exh)
             throw k2(UNIT)
  let fls : FLS.fls = FLS.@get(/ exh)    
  do VProcQueue.@enqueue(fls, k1 / exh)
  throw k2(UNIT)
;

define @test4 (x : unit / exh : exh) : unit =
  let x : ![int] = alloc(0)
  let x : ![int] = promote(x)
  cont k2 (y : unit) =
      do print_ppt()
      let x : int = I32FetchAndAdd (&0(x), 1)
      let _ : unit = SchedulerAction.@stop()
	    do print_ppt()
	    let e : exn = Match
	    throw exh(e)
      
  let fls : FLS.fls = FLS.@get(/ exh)
  let vp : vproc = VProc.@id-of-vproc(0 / exh)
  do VProcQueue.@enqueue-on-vproc(vp, fls, k2)
  fun isDone (/ exh : exh) : bool = return(I32Eq(#0(x), 1))
  do PrimSynch.@spin-wait(isDone / exh)
  return(UNIT)
;

*)

define @test5 (x : unit / exh : exh) : unit =
  let barrier : NWayBarrier.barrier = NWayBarrier.@new(1 / exh)  
  cont k2 (y : unit) =
      do print_ppt()
      do NWayBarrier.@ready(barrier / exh)
      do NWayBarrier.@barrier(barrier / exh)
      let _ : unit = SchedulerAction.@stop()
	    do print_ppt()
	    let e : exn = Match
	    throw exh(e)
      
  let fls : FLS.fls = FLS.@get(/ exh)
  let vp : vproc = VProc.@id-of-vproc(0)
  do VProcQueue.@enqueue-on-vproc(vp, fls, k2)
  do NWayBarrier.@barrier(barrier / exh)
  return(UNIT)
;

define @test6 (x : unit / exh : exh) : unit =
  let fls : FLS.fls = FLS.@get(/ exh)
  cont k (x : unit) =
    do print_ppt()
    SchedulerAction.@stop()
  fun spawnFn (i : int, k : PT.fiber / exh : exh) : () =
      let vp : vproc = VProc.@id-of-vproc(i)
      let fls : FLS.fls = FLS.@pin-to(fls, i / exh)
      VProcQueue.@enqueue-on-vproc(vp, fls, k)
  let n : int = VProc.@num-vprocs()
  do ImplicitThread.@spawn-n-workers(n, k, spawnFn / exh)
  return(UNIT)
;


)

val test : unit -> unit = _prim(@test6)
val () = test()
val () = Print.printLn "implicit-thread-test: finished"
