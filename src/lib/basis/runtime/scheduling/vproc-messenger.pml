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

      define @send (vp : vproc, k : PT.fiber / exh : PT.exh) : () =
	VPQ.@enqueue-on-vproc(vp, MESSENGER_FLS, k / exh)  
      ;

    )

  end
