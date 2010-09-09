(* thread-ops.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Operations for managing explicit threads.
 *)

#include "runtime-offsets.def"

structure ThreadOps : sig end =
  struct

    structure PT = PrimTypes

    _primcode(

(* FIXME: this mechanism belong in the CML library *)
    (* put the running thread to sleep for some amount of time.
     * duration is the minimum time to sleep and termFlg can
     * be used to cancel the wakeup.
     *)
      define @sleep (duration : Time.time, termFlg : ![PT.bool] / exh : PT.exh) : () =
	let t0 : Time.time = Time.@now ()
	let finishTime : Time.time = Time.@add (t0, duration / exh)
	fun lp ( / exh : PT.exh) : () =
	    let t : Time.time = Time.@now()
            let finished : PT.bool = Time.@gt(t, finishTime / exh)
	    if finished
		then return ()
	    else if #0(termFlg)
		then return ()
	    else
		do SchedulerAction.@yield ()
		apply lp ( / exh)
	 apply lp ( / exh)
      ;

(* Testing code: belongs somewhere else *)
    (* sleep for a randomly determined amount of time *)
      define @rand-sleep (maxSleepTime : Time.time, done : ![PT.bool] / exh : PT.exh) : () =
        let len : long = Rand.@in-range-long(0:long, maxSleepTime / exh)
        do @sleep(len, done / exh)
        return()
      ;

    )

  end
