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

    _primcode(
        define @init-counter(x : unit / exh : exh) : any = 
            let x : ![int] = alloc(0)
            let x : ![int] = promote(x)
            return(x);
    )

    val init : unit -> 'a = _prim(@init-counter)
    val counter = init()
    fun getCounter() = counter

    _primcode (

        extern void * M_Print_Int(void *, int);

        define @get-default-implicit-thread-sched = getDefaultImplicitThreadSched;

      (* create a thread *)
	define inline @create (f : fun(PT.unit / PT.exh -> PT.unit) / exh : PT.exh) : PT.fiber =
	    cont fiber (x : PT.unit) = 
	      let x : PT.unit =
	      (* in case of an exception, just terminate the fiber *)
		cont exh (exn : PT.exn) = return (UNIT)
		(* in *)
		let defaultImplicitThreadSched : ImplicitThread.work_group = 
						   @get-default-implicit-thread-sched (UNIT / exh)
                let _ : unit = ImplicitThread.@default-work-group-begin (defaultImplicitThreadSched / exh)
                apply f (UNIT / exh)
	      (* in *)
		SchedulerAction.@stop ()
	    (* in *)
	    return (fiber)
	  ;

      (* spawn a new thread on the local vproc.
       * NOTE: the name "@local-spawn" is baked into the compiler (see the file
       * mc/translate/translate.sml), so do not change it!
       *)
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
		then VProcQueue.@enqueue-in-atomic (self, fls, f)
		else VProcQueue.@enqueue-on-vproc-in-atomic (self, dst, fls, f)
	  ;

      (* spawn a thread on a remote vproc *)
	define @remote-spawn (dst : vproc, f : fun (unit / exh -> unit) / exh : exh) : FLS.fls =
	    let fiber: PT.fiber = @create (f / exh)
	    let vprocId : int = VProc.@vproc-id (dst)
	    let fls : FLS.fls = FLS.@new-pinned (vprocId)
	    do @enqueue-ready (host_vproc, dst, fls, fiber)
	    return (fls)
	  ;

        define @getCounter = getCounter;

        define @spawn-eq(f : fun(unit / exh -> unit) / exh:exh) : unit = 
                let counter : ![int] = @getCounter(UNIT / exh)
                let i : int = I32FetchAndAdd(&0(counter), 1)
                let numVPs : int = VProc.@num-vprocs()
                let vp : int = I32Mod(i, numVPs)
                cont fiber(x:PT.unit) = 
                    cont threadExh(e:PT.exn) = 
                        case e
                            of Fail(s:ml_string) => 
                                do ccall M_Print("Thread exiting because of uncaught exception: ")
                                do ccall M_Print(#0(s))
                                return(UNIT)
                            | _ => do ccall M_Print("Thread exiting because of uncaught exception\n")
                                   return(UNIT)
                       end                 
                    let _ : unit = apply f(UNIT / threadExh)
                    SchedulerAction.@stop()
                let fls : FLS.fls = FLS.@new-pinned(vp)
                let fls : FLS.fls = promote(fls)
                let self : vproc = host_vproc
                let dst : vproc = VProc.@vproc-by-id(vp)
                do @enqueue-ready(self, dst, fls, fiber)
                return(UNIT);


        define @spawn-on(arg : [[int],fun(unit / exh -> unit)] / exh:exh) : unit = 
                let vp : [int] = #0(arg)
                let f : fun(unit / exh -> unit) = #1(arg)
                cont fiber(x:PT.unit) = 
                    cont threadExh(e:PT.exn) = 
                        case e
                            of Fail(s:ml_string) => 
                                do ccall M_Print("Thread exiting because of uncaught exception: ")
                                do ccall M_Print(#0(s))
                                SchedulerAction.@stop()
                            | _ => do ccall M_Print("Thread exiting because of uncaught exception\n")
                                   return(UNIT)
                       end                 
                    let _ : unit = apply f(UNIT / threadExh)
                    SchedulerAction.@stop()
                let fls : FLS.fls = FLS.@new-pinned(#0(vp))
                let self : vproc = host_vproc
                let dst : vproc = VProc.@vproc-by-id(#0(vp))
                do @enqueue-ready(self, dst, fls, fiber)
                return(UNIT);

	define inline @yield (_ : unit / _ : exh) : unit =
	    do SchedulerAction.@yield ()
	    return (UNIT)
	  ;

	define inline @thread-exit (x : PT.unit / exh : PT.exh) : any =
	    SchedulerAction.@stop ()
	  ;

      )

    val yield : unit -> unit = _prim(@yield)
    val spawnOn : int * (unit -> unit) -> unit = _prim(@spawn-on)
    val exit : unit -> 'a = _prim(@thread-exit)
    val spawnEq : (unit -> unit) -> unit = _prim(@spawn-eq)

  end

