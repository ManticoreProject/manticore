structure ThreadOps =
  struct

#include "runtime-offsets.def"

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
(*	    else if #0(termFlg)
	       then return()*)
	    else
		let _ : PT.unit = Control.@yield(/exh)
		apply lp( / exh)
	 apply lp( / exh)
      ;

    )
  end
