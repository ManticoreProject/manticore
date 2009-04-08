(* vproc-pingpong.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Benchmark to measure the latency of signaling vprocs. The input is the number
 * of rounds, which involves twice that many signals.
 *)

structure VProcPingPong =
  struct

    _primcode(

      define @run (arg : [[int], [vproc]] / exh : exh) : unit =
	  let n : int = #0(#0(arg))
	  let vp1 : vproc = SchedulerAction.@atomic-begin()
	  let vp2 : vproc = #0(#1(arg))

	  let n1 : ![int] = alloc(0)
	  let n1 : ![int] = promote(n1)
	  let n2 : ![int] = alloc(0)
	  let n2 : ![int] = promote(n2)

          fun wait (vp : vproc, x : int, n : ![int]) : () =
	      if I32Eq(x, #0(n))
		 then 
		  do SchedulerAction.@yield-in-atomic (vp)
		  apply wait (vp, x, n)
	      else
		  return()

	  fun ping () : () =
	      let i : int = #0(n1)
	      if I32Lt(i, n)
		 then
		  cont k (_ : unit) =
		      do #0(n2) := I32Add(#0(n2), 1)
		      do SchedulerAction.@stop()
		      return()
		  do VProc.@send-high-priority-signal-from-atomic(vp1, vp2, k)
		  do apply wait (vp1, i, n1)
		  apply ping ()
	      else 
		  return()

	  fun pong (vp2 : vproc, i : int) : () =
	      if I32Lt(i, n)
		 then
		  do apply wait (vp2, i, n2)
		  cont k (_ : unit) =
		      do #0(n1) := I32Add(#0(n1), 1)
		      do SchedulerAction.@stop()
		      return()
		  do VProc.@send-high-priority-signal-from-atomic(vp2, vp1, k)
		  apply pong (vp2, I32Add(i, 1))
	      else
		  let _ : unit = SchedulerAction.@stop()
		  return()

	  cont k2 (_ : unit) =
              let vp2 : vproc = SchedulerAction.@atomic-begin()
	      do apply pong (vp2, 0)
	      SchedulerAction.@stop()

	  do VProc.@send-high-priority-signal-from-atomic(vp1, vp2, k2)
	  do apply ping ()

	  do SchedulerAction.@atomic-end(vp1)
	  return(UNIT)
	;

    )

    val run : (int * VProcExtras.vproc) -> unit = _prim(@run)

  end

structure Main =
  struct

    val otherVP = List.hd(List.tl(VProcExtras.vprocs()))

    fun timeit n = let
	  val t0 = Time.now()
	  val () = VProcPingPong.run (n, otherVP)
	  val t = (Time.now() - t0)
	  in
	    Print.print (String.concat[
		"{messages=", Int.toString n,
		" seconds=", Time.toString t,
		"}"
	      ])
	  end

  end

val _ = Main.timeit (PrimIO.readInt())
