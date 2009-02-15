(* threads.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Threads : sig

  end = struct

    structure PT = PrimTypes

    _primcode (
      (* spawn a new CML thread on the local vproc *)
	define inline @local-spawn (f : fun(PT.unit / PT.exh -> PT.unit) / exh : PT.exh) : FLS.fls =
	  cont fiber (x : PT.unit) = 
	    let x : PT.unit =
	    (* in case of an exception, just terminate the fiber *)
	      cont exh (exn : PT.exn) = return (UNIT)
	      (* in *)
		apply f (UNIT / exh)
	    (* in *)
	      SchedulerAction.@stop ( / exh)
	  (* in *)
	  let vp : vproc = host_vproc
	  let fls : FLS.fls = FLS.@new (UNIT / exh)
	  do VProcQueue.@atomic-enqueue (fls, fiber / exh)
	  return (fls)
	;

	define inline @thread-exit (x : PT.unit / exh : PT.exh) : any =
	  SchedulerAction.@stop ( /exh)
	;

      )

    val exit : unit -> 'a = _prim(@thread-exit)

  end

