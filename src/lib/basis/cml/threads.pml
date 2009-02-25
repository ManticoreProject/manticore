(* threads.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Threads (*: sig

    type thread_id
    type vproc

    val exit : unit -> 'a

  end*) = struct

    structure PT = PrimTypes

    type thread_id = _prim(FLS.fls)
    type vproc = _prim(vproc)

    _primcode (

      (* create a thread *)
	define inline @create (f : fun(PT.unit / PT.exh -> PT.unit) / exh : PT.exh) : PT.fiber =
	    cont fiber (x : PT.unit) = 
	      let x : PT.unit =
	      (* in case of an exception, just terminate the fiber *)
		cont exh (exn : PT.exn) = return (UNIT)
		(* in *)
		  apply f (UNIT / exh)
	      (* in *)
		SchedulerAction.@stop ()
	    (* in *)
	    return (fiber)
	  ;

      (* spawn a new thread on the local vproc *)
	define inline @local-spawn (f : fun(PT.unit / PT.exh -> PT.unit) / exh : PT.exh) : FLS.fls =
	    let fiber: PT.fiber = @create (f / exh)
	    let fls : FLS.fls = FLS.@new (UNIT / exh)
	    (* in *)
	    do VProcQueue.@enqueue (fls, fiber)
	    return (fls)
	  ;

      (* enqueue a thread on the given vproc, checking to see if it is the same
       * host vproc.
       *)
	define inline @enqueue-ready (self : vproc, dst : vproc, fls : FLS.fls, f : PT.fiber) : () =
	    if Equal(self, dst)
		then VProcQueue.@enqueue (fls, f)
		else VProcQueue.@enqueue-on-vproc (dst, fls, f)
	  ;
	define inline @enqueue-ready-in-atomic (self : vproc, dst : vproc, fls : FLS.fls, f : PT.fiber) : () =
	    if Equal(self, dst)
		then VProcQueue.@enqueue-in-atomic (fls, f)
		else VProcQueue.@enqueue-on-vproc (dst, fls, f)
	  ;

      (* spawn a thread on a remote vproc *)
	define @remote-spawn (dst : vproc, f : fun (unit / exh -> unit) / exh : exh) : FLS.fls =
	    let fiber: PT.fiber = @create (f / exh)
	    let vprocId : int = VProc.@vproc-id (dst)
	    let fls : FLS.fls = FLS.@new-pinned (vprocId)
	    do @enqueue-ready (host_vproc, dst, fls, fiber)
	    return (fls)
	  ;

	define inline @yield (_ : unit / _ : exh) : unit =
	    do SchedulerAction.@yield ()
	    return (UNIT)
	  ;

	define inline @thread-exit (x : PT.unit / exh : PT.exh) : any =
	    SchedulerAction.@stop ()
	  ;

      )

    val yield : unit -> unit = _prim(@yield)

    val exit : unit -> 'a = _prim(@thread-exit)

  end

