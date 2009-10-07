(* test-runtime.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Some standard tests of the Manticore runtime framework.
 *)

structure PT = PrimTypes 

(**** scheduler actions ****)

_primcode(

define @null-sched-act (x : ![int]) : PT.sched_act =
    cont act (signal : PT.signal) =
      case signal
       of PT.STOP =>
	  let _ : unit = SchedulerAction.@stop()
          return($0)
	| PT.PREEMPT(k : PT.fiber) =>
	  do UPDATE(0, x, I32Add(SELECT(0, x), 1))
          do SchedulerAction.@yield-in-atomic(host_vproc)
          let _ : unit = SchedulerAction.@stop()
          return($0)
	| _ => return($0)
      end
    return(act)
;

(* test assumes that asynchronous signals are disabled *)
define @test-sched-act(n : [int] / exh : exh) : bool =
  let x : ![int] = alloc(0)
  let x : ![int] = promote(x)

  let n : int = #0(n)

  fun run (i : int) : () =
      if I32Lte(i, 0)
      then return()
      else 
	  cont k (x : unit) = apply run (I32Sub(i, 1))
          let act : PT.sched_act = @null-sched-act(x)
          SchedulerAction.@run(host_vproc, act, k)

  cont act (sign : PT.signal) = 
    case sign
     of PT.STOP => return(I32Eq(SELECT(0, x), n))
      | PT.PREEMPT(k : PT.fiber) =>
	return(I32Eq(SELECT(0, x), n))
      | _ =>
	let e : exn = Match
     	throw exh(e)
    end
  
  do cont k (z : unit) = return()
     SchedulerAction.@run(host_vproc, act, k)

  do apply run (n)

  do SchedulerAction.@yield()
  return(I32Eq(SELECT(0, x), n))
;

)

val testSchedAct : int -> bool = _prim(@test-sched-act)
val () = pml_assert(testSchedAct 15)

fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1));

(**** vproc-queue tests ****)

_primcode(

define @run-remote-fiber (dst : vproc) : () =
  let x : ![bool] = alloc(false)
  let x : ![bool] = promote(x)
  cont k (z : unit) = 
    do UPDATE(0, x, true)
    let _ : unit = SchedulerAction.@stop()
    return()
  let fls : FLS.fls = FLS.@get()
  do VProcQueue.@enqueue-on-vproc(dst, fls, k)
  fun spin () : () =
      if #0(x)
	 then return()
      else apply spin()
  apply spin()
;

define @spread-across-vprocs (x : unit / exh : exh) : unit =
  fun f (vp : vproc / exh : exh) : () =
      @run-remote-fiber(vp)
  let vp : vproc = SchedulerAction.@atomic-begin()
  do VProc.@for-other-vprocs-from-atomic(f / exh)
  do SchedulerAction.@atomic-end(vp)
  return(UNIT)
;

)

val spreadAcrossVProcs : unit -> unit = _prim(@spread-across-vprocs)
fun testEnqueueOnVProc nIter =
    if nIter < 0
       then ()
    else (
	delay 20;
	spreadAcrossVProcs();
	testEnqueueOnVProc(nIter-1))

val () = testEnqueueOnVProc 15

val () = Print.printLn "Runtime tests finished"
