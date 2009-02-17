(* threads.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Threads : sig

  end = struct

    structure PT = PrimTypes

    _primcode (

      (* create a thread *)
	define inline @create (f : fun(PT.unit / PT.exh -> PT.unit) / exh : PT.exh) : (FLS.fls, PT.fiber) =
	    cont fiber (x : PT.unit) = 
	      let x : PT.unit =
	      (* in case of an exception, just terminate the fiber *)
		cont exh (exn : PT.exn) = return (UNIT)
		(* in *)
		  apply f (UNIT / exh)
	      (* in *)
		SchedulerAction.@stop ()
	    (* in *)
	    let fls : FLS.fls = FLS.@new (UNIT / exh)
	    return (fls, fiber)
	  ;

      (* spawn a new thread on the local vproc *)
	define inline @local-spawn (f : fun(PT.unit / PT.exh -> PT.unit) / exh : PT.exh) : FLS.fls =
	    let (fls : FLS.fls, fiber: PT.fiber) = @create (f / exh)
	    (* in *)
	    do VProcQueue.@enqueue (fls, fiber / exh)
	    return (fls)
	  ;

      (* spawn a thread on a remote vproc *)
	define @remote-spawn (dst : vproc, f : fun (unit / exh -> unit) / exh : exh) : FLS.fls =
	    let (fls : FLS.fls, fiber: PT.fiber) = @create (f / exh)
	    (* in *)
	    do VProcQueue.@enqueue-on-vproc (dst, fls, fiber / exh)
	    return ()
	  ;

	define inline @thread-exit (x : PT.unit / exh : PT.exh) : any =
	    SchedulerAction.@stop ()
	  ;

      )

    val exit : unit -> 'a = _prim(@thread-exit)

  end

