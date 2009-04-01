(* vproc-init.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Virtual processor initialization.
 *)


structure VProcInit (* :
  sig

    _prim(

    (* bootstrap the vproc.
     *   - mkAct is a function that takes a vproc and returns the top-level scheduler
     *     for that vproc.
     *)
      define @bootstrap (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : ();

    )

  end *) = struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode (

      define @bootstrap (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
	  let self : vproc = SchedulerAction.@atomic-begin()
        (* initialize fields in the vproc structure *)
          fun initVPFields (vp : vproc / exh : exh) : () =
              (**** vp->schedCont ****)
		cont schedCont (k : PT.fiber) = 
		    do assert(NotEqual(k, nil))
		    SchedulerAction.@forward(PT.PREEMPT(k))
 	        let schedCont : cont(PT.fiber) = promote(schedCont)
		let currentSchedCont : cont(PT.fiber) = vpload(VP_SCHED_CONT, vp)
		do assert(Equal(currentSchedCont, nil))
		do vpstore(VP_SCHED_CONT, vp, schedCont)
              (**** vp->currentFLS ****)
	        let fls : FLS.fls = FLS.@new(UNIT / exh)
   	        let fls : FLS.fls = promote(fls)
	        do vpstore(CURRENT_FLS, vp, fls)
              (**** vp->dummyK ****)
	        cont dummyK (x : unit) = 
	            let _ : unit = SchedulerAction.@stop()
	            return()
	        let dummyK : PT.fiber = promote(dummyK)
	        do vpstore(VP_DUMMYK, vp, dummyK)
              (**** vp->actionStk ****)
                let act : PT.sched_act = apply mkAct (vp / exh)
		do assert(NotEqual(act, nil))
		let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
		let item : [PT.sched_act, any] = alloc (act, (any)stk)
		let item : [PT.sched_act, any] = promote (item)
		do vpstore (VP_ACTION_STK, vp, item)
                return()
          do VProc.@for-each-vproc(initVPFields / exh)

          cont startLeadK (_ : PT.unit) = return()
	  let act : PT.sched_act = apply mkAct (self / exh)
	  SchedulerAction.@run(self, act, startLeadK)
      ;

    )

  end
