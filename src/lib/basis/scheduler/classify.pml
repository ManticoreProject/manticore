(* classify.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Classify =
  struct

    structure PT = PrimTypes
    structure VPQ = VProcQueue

#include "vproc-queue.def"

    _primcode(

    (* thread classification library for computationally intensive and interactive threads *)

    (* check the doneComm flag, to see if the thread needs to be demoted in the case of an involuntary processor yield
       this must be called *before* update-done-comm *)
      define @demote-test-in-atomic ( / exh : exh) : bool =
	let w : bool = FLS.@get-done-comm(/ exh)
	if (w) then return (false)
	else return (true)
      ;

    (* update the doneComm flag, based on a voluntary or involuntary yield of the processor *)
      define @update-done-comm-in-atomic (voluntary : bool / exh : exh) : () =
	let _ : unit = FLS.@set-done-comm(voluntary / exh)
	return ()
      ;

    (* move threads based on a voluntary processor yield
       this must be called *before* update-done-comm *)
      define @move-threads-in-atomic (self : vproc / exh : exh) : () =
	let w : bool = FLS.@get-done-comm(/ exh)
	if (w) then
		let item : Option.option = VPQ.@secondary-dequeue-in-atomic(self)
		case item
		  of Option.NONE => return ()
		   | Option.SOME(qitem : queue_item) =>
		     do VPQ.@enqueue-in-atomic (self, SELECT(FLS_OFF, qitem), SELECT(FIBER_OFF, qitem))
		     return ()
		end
	else return ()
      ;

    (* handles done-comm based on the type of processor yield, for all cases where the scheduler can safely reschedule the thread
       returns the results of the demote test *)
      define @done-comm-ops-in-atomic (self : vproc, voluntary : bool / exh : exh) : bool = 
	let w : bool = FLS.@get-done-comm(/ exh)
	if (voluntary) then
		do @move-threads-in-atomic(self / exh)
		do @update-done-comm-in-atomic(voluntary / exh)
		return(false)
	else
		let v : bool = @demote-test-in-atomic(/ exh)
		do @update-done-comm-in-atomic(voluntary / exh)
		return (v)
      ;
    )

  end
