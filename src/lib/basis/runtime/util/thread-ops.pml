(* thread-ops.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations for managing explicit threads.
 *)

#include "runtime-offsets.def"

structure ThreadOps =
  struct

    structure PT = PrimTypes

    _primcode(

    (* put the running thread to sleep for some amount of time *)
      define @sleep(duration : Time.time,     (* minimum time to sleep *)
		    termFlg : ![PT.bool]      (* cancel the wake up *)
		    / exh : PT.exh) : () =
	let t0 : Time.time = Time.@now(/exh)
	let finishTime : Time.time = Time.@add(t0, duration / exh)
	fun lp ( / exh : PT.exh) : () =
	    let t : Time.time = Time.@now(/exh)
            let finished : PT.bool = Time.@gt(t, finishTime / exh)
	    if finished
	       then return()
	    else if #0(termFlg)
	       then return()
	    else
		let _ : PT.unit = Control.@yield(/exh)
		apply lp( / exh)
	 apply lp( / exh)
      ;

    (* sleep for a randomly determined amount of time *)
      define @rand-sleep (maxSleepTime : Time.time, done : ![PT.bool] / exh : PT.exh) : () =
        let len : long = Rand.@in-range-long(0:long, maxSleepTime / exh)
        do @sleep(len, done / exh)
        return()
      ;

    )

  end
