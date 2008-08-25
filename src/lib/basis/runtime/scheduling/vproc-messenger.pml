(* vproc-messenger.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Carry messages across VProcs using software polling.
 *)

#include "vproc-queue.def"

#define EMPTY_STATE    $0

#define STATE_OFF      0
#define FIBER_OFF      1

structure VProcMessenger =
  struct

    structure PT = PrimTypes
    structure VPQ = VProcQueue

    _primcode (

    (* messenger channel *)
      typedef chan = ![
		  any,         (* state word *)
		  PT.fiber     (* messenger fiber *)
	      ];

    (* takes a thunk and creates a messenger channel *)
      define @new (f : fun(PT.unit / PT.exh -> Option.option) / exh : PT.exh) : chan =
	let ch : chan = alloc(EMPTY_STATE, EMPTY_STATE)
	let ch : chan = promote(ch)
	fun wrapper (x : PT.unit / exh : PT.exh) : PT.unit =
	    do if Equal(SELECT(STATE_OFF, ch), EMPTY_STATE)
		  then let x : Option.option = apply f(UNIT / exh)
		       let x : Option.option = promote(x)
		       do UPDATE(STATE_OFF, ch, (any)x)
		       return()
	       else return()
	    return(UNIT)
	let k : PT.fiber = Control.@fiber(wrapper / exh)
	let k : PT.fiber = promote(k)
	do UPDATE(FIBER_OFF, ch, k)
	return(ch)
      ;

    (* send the messenger *)
      define @send (ch : chan, vp : vproc / exh : PT.exh) : () =
	let isVPIdle : PT.bool = SchedulerUtils.@is-vp-idle(vp / exh)
	if isVPIdle
	   then
	    do UPDATE(STATE_OFF, ch, (any)NONE)
	    return()
	else 
	    VPQ.@enqueue-on-vproc(vp, MESSENGER_FLS, SELECT(FIBER_OFF, ch) / exh)
      ;

    (* receive the messenger's response *)
      define @recv (ch : chan / exh : PT.exh) : Option.option =
      (* spin until the messenger fiber has completed *)
	fun lp () : Option.option =
	    if Equal(SELECT(STATE_OFF, ch), EMPTY_STATE)
	       then apply lp()
	    else return((Option.option)SELECT(STATE_OFF, ch))
	apply lp()
      ;

    )

  end
