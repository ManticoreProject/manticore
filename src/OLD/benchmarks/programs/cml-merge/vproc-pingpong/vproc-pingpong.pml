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
	  let vp1 : vproc = host_vproc
	  let vp2 : vproc = #0(#1(arg))

	  let n1 : ![int] = alloc(0)
	  let n1 : ![int] = promote(n1)
	  let n2 : ![int] = alloc(0)
	  let n2 : ![int] = promote(n2)

          fun wait (x : int, n : ![int]) : () =
	      if I32Eq(x, #0(n))
		 then 
		  apply wait (x, n)
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
                  let self : vproc = SchedulerAction.@atomic-begin()
		  do VProc.@send-high-priority-signal-from-atomic(vp1, vp2, k)
                  do SchedulerAction.@atomic-end(self)
		  do apply wait (i, n1)
		  apply ping ()
	      else 
		  return()

	  fun pong (i : int) : () =
	      if I32Lt(i, n)
		 then
		  do apply wait (i, n2)
		  cont k (_ : unit) =
		      do #0(n1) := I32Add(#0(n1), 1)
		      do SchedulerAction.@stop()
		      return()
                  let self : vproc = SchedulerAction.@atomic-begin()
		  do VProc.@send-high-priority-signal-from-atomic(vp2, vp1, k)
                  do SchedulerAction.@atomic-end(self)
		  apply pong (I32Add(i, 1))
	      else
		  return()

	  cont k2 (_ : unit) =
	      do apply pong (0)
	      SchedulerAction.@stop()

          let self : vproc = SchedulerAction.@atomic-begin()
	  do VProc.@send-high-priority-signal-from-atomic(vp1, vp2, k2)
          do SchedulerAction.@atomic-end(self)
	  do apply ping ()

	  return(UNIT)
	;

    )

    val run : (int * VProcExtras.vproc) -> unit = _prim(@run)

  end

structure Main =
  struct

    val dfltN = 100000

    val otherVP = List.hd(List.tl(VProcExtras.vprocs()))
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () = VProcPingPong.run (n, otherVP)		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
