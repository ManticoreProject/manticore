(* vproc-ping-busy.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Benchmark to measure the latency of signaling vprocs. Here we signal a vproc that is already
 * working on a computation.
 *)

structure VProcPingBusy =
  struct

    _primcode(

      define @ping (vp : [vproc] / exh : exh) : unit =
	  let self : vproc = SchedulerAction.@atomic-begin()
	  let vp : vproc = #0(vp)
	  let resp : ![bool] = alloc(false)
	  let resp : ![bool] = promote(resp)

	  cont k (_ : unit) =
	    do #0(resp) := true
	    let _ : unit = SchedulerAction.@stop()
            do assert(false)
            return(UNIT)

	  fun wait () : () =
	      if #0(resp)
		 then 
		  return()
	      else
		  do SchedulerAction.@yield-in-atomic (self)
		  apply wait()

          do VProc.@send-high-priority-signal-from-atomic(self, vp, k)
	  do apply wait()
	  do SchedulerAction.@atomic-end(self)
	  return(UNIT)
	;

    )

    val ping : VProcExtras.vproc -> unit = _prim(@ping)

    val otherVP = List.hd(List.tl(VProcExtras.vprocs()))

    fun run n = 
	  if n = 0
	     then 
	      ()
	  else (
	      ping otherVP;
	      run (n - 1))

  end

structure Main =
  struct

    fun delay n = if (n <= 0) then () else (delay(n-1); delay(n-1))

    _primcode(
      define @mk-busy (vp : [vproc] / exh : exh) : unit =
	  cont k (_ : unit) = throw k(UNIT)
	  do VProc.@send-high-priority-signal-from-atomic(host_vproc, #0(vp), k)
	  return(UNIT)
        ;
    )

    val mkBusy : VProcExtras.vproc -> unit = _prim(@mk-busy)

    fun timeit n = let
 	  val () = delay 18
	  val () = mkBusy VProcPingBusy.otherVP
	  val t0 = Time.now()
	  val () = VProcPingBusy.run n
	  val t = (Time.now() - t0)
	  in
	    Print.print (String.concat[
		"{messages=", Int.toString n,
		" seconds=", Time.toString t,
		"}"
	      ])
	  end

  end

val _ = Main.timeit 10
