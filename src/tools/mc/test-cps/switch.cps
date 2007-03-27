(* switch.cps *)

module Switch
  extern void M_Print (void *);
  fun init (wi : [int], kRet : cont(enum(0)), exh : cont(any)) =

    let stop : enum(0) = 0
    let dummyTid : enum(0) = 0
    let limit : long = 1000000000

    let retVal : [int] = wrap(1:int)

    fun thread_exit (x : enum(0), retK : cont(any), exh : cont(any)) =
          do ccall M_Print ("thread exiting\n\000")
	  let vp : vproc = host_vproc
	    forward vp stop

    fun spawn (f : fun(enum(0), cont(any), cont(any)), retK : cont(enum(0)), exh : cont(any)) =
	  cont fiber (x : enum(0)) =
		cont ret (y : enum(0)) = apply thread_exit((enum(0))0, retK, exh)
		apply f (x, ret, exh)
	  do enqueue(host_vproc, dummyTid, fiber)
	  throw retK ((enum(0))0)

    fun loop (x : long, k : cont(any), exh : cont(any)) =
	  if I64Eq(x, limit)
	    then throw k ((enum(0))0)
	    else apply loop(I64Add(x, 1:long), k, exh)

    fun thread (arg : enum(0), retK : cont(any), exh : cont(any)) =
          do ccall M_Print ("thread starting\n\000")
	  apply loop (0:long, retK, exh)

   (* action is a scheduler action that terminates upon a stop
    * signal, and resumes upon a preempt signal.
    *)
    cont action (fiber : cont(enum(0))) =
	  let vp : any = host_vproc
          if I64Eq (fiber, stop)
	    then
              do ccall M_Print ("handle STOP\n\000")
	      let item : (any, cont(enum(0)), any) = dequeue(vp)
	      let nextFiber : cont(enum(0)) = #1(item)
	      run vp action nextFiber
	    else 
              do ccall M_Print ("handle PREEMPT\n\000")
	      do enqueue(vp, (enum(0))0, fiber)
	      let item : (any, cont(enum(0)), any) = dequeue(vp)
	      let nextFiber : cont(enum(0)) = #1(item)
	      run vp action nextFiber

    cont fiber (x : any) =
	fun lp (i : int, kRet : cont(any), exh : cont(any)) =
	      if I32Eq(i, 0:int)
		then apply thread_exit ((enum(0))0, kRet, exh)
		else cont k (y : any) = apply lp(I32Sub(i, 1:int), kRet, exh)
		  apply spawn(thread, k, exh)
	apply lp(4:int, kRet, exh)

  (* start the initial fiber *)
    let vp : vproc = host_vproc
    run vp action fiber
