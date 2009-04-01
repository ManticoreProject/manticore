(* implicit-thread.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Underlying support for implicitly-generated threads.
 *)

structure ImplicitThread (* :
  sig

    type group

  (* evaluate a thunk on a work group. *)
    val runWithGroup : group * (unit -> 'a) -> 'a
  (* get a reference to the group at the top of the work-group stack *)
    val currentGroup : unit -> group
  (* begin the dynamic scope of a group *)
    val groupBegin : group -> unit

    _prim(

(* FIXME: explain the removeFn function *)
    (* create a work group and make it ready to receive work *)
      define @group (workerInit : PT.fiber, spawnFn : fun(thread / exh -> unit) / exh : exh) : group;

    (* allociate an implicit thread *)
      define inline @alloc (ite : FLS.ite, k : PT.fiber / exh : exh) : thread;

    (* create an implicit thread *)
      define inline @thread (k : PT.fiber, 
		      c : Option.option           (* cancelable *)
                     / exh : exh) : thread;

    (* construct an implicit thread. we obtain the environment by capturing the
     * implicit environment.
     *) 
      define inline @capture (k : PT.fiber           (* fiber for implicit thread *)
	          	      / exh : exh) : thread;
    
    (* run an implicit thread outside of a scheduler action *)
      define inline @run-out-of-scheduler (thd : thread / exh : exh) noreturn;

    (* run an implicit thread from within a scheduler action *)
      define inline @run-in-scheduler (vp : vproc, sched : PT.sched_act, thd : thread / exh : exh) noreturn;

    (* spawn an implicit thread on the work group at the top of the work-group stack *)
      define inline @spawn (thd : thread / exh : exh) : ();

    (* remove a thread from the ready queue *)
      define inline @remove-thread (thd : ImplicitThread.thread / exh : exh) : bool =

    (* get the scheduler data for current group *)
      define inline @get-scheduler-data (/ exh : exh) : any;

    )

  end *) = struct

    structure PT = PrimTypes

    _primcode(

#define ITE_STACK_OFF         0
#define ITE_CANCELABLE_OFF    1

    (* environment for each implicit thread *)
      typedef ite = FLS.ite;

#define FIBER_OFF   0
#define ITE_OFF     1

    (* representation for a suspended implicit thread: suspended thread + environment *)
      typedef thread = [
		  PT.fiber,                       (* suspended thread *)
		  ite
       ];

#define GROUP_WORKER_INIT_OFF         0
#define GROUP_SPAWN_OFF               1
#define GROUP_REMOVE_OFF              2
#define GROUP_SCHEDULER_DATA_OFF      3

    (* representation for a work group *)
      typedef group = [
		PT.fiber,                      (* worker initialization *)
		fun(thread / exh -> unit),     (* place a thread on the ready queue *)
		fun(thread / exh -> bool),     (* remove a thread from the ready queue; returns true 
						* iff the thread had been migrated off the queue.
						*)
		any                            (* scheduler data *)
      ];

    (* migrate the current thread to the top-level thread scheduler. the effect of this operation is to
     * escape from any nested scheduler instance.
     *)
      define inline @migrate-to-top-level-sched () : () = 
	cont k (x : unit) = return()
	let fls : FLS.fls = FLS.@get()
	do VProcQueue.@enqueue(fls, k)
	let _ : unit = SchedulerAction.@stop()
        return()
      ;

    (* access the work-group stack *)
      define inline @init-ite ( / exh : exh) : () =
        let iteOpt : Option.option = FLS.@find-ite(/ exh)
        case iteOpt
	 of Option.NONE =>
	    (* if there is no current ite, create a new one and set it as current *)
	    let ite : ite = FLS.@ite(List.nil, Option.NONE / exh)
	    do FLS.@set-ite(ite / exh)
            return()
	  | Option.SOME (ite : ite) =>
	    let ite : ite = FLS.@get-ite(/ exh)
 	    return()
        end
      ;

    (* return the top of the group stack.
     * NOTE: an exception is raised if the stack is empty
     *)
      define inline @peek (x : unit / exh : exh) : group =
        let ite : ite = FLS.@get-ite(/ exh)
        let stk : List.list = SELECT(ITE_STACK_OFF, ite)
        case stk
	 of List.nil => 
	    let e : exn = Fail(@"ImplicitThread.@peek: empty work-group stack")
            throw exh(e)
	  | List.CONS(group : group, stk : List.list) =>
	    return(group)
        end
      ;

      define inline @pop ( / exh : exh) : Option.option =
	let ite : ite = FLS.@get-ite( / exh)
        let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	case stk
	 of List.nil => 
	    return(Option.NONE)
	  | List.CONS(x : any, stk : List.list) => 
	    let ite : ite = alloc(stk, SELECT(ITE_CANCELABLE_OFF, ite))
	    do FLS.@set-ite(ite / exh)
	    return(Option.SOME(x))
	end
      ;

      define inline @push (group : group / exh : exh) : () =
	let ite : ite = FLS.@get-ite( / exh)
        let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	let stk : List.list = List.CONS(group, stk)
	let ite : ite = alloc(stk, SELECT(ITE_CANCELABLE_OFF, ite))
	do FLS.@set-ite(ite / exh)
	return()
      ;

    (* initiate n worker fibers on allocated processors.
     * POSTCONDITION: each worker has started running
     *)
      define @spawn-n-workers (nWorkers : int, k : PT.fiber, spawnFn : fun(int, PT.fiber / exh -> ) / exh : exh) : () =
	let barrier : NWayBarrier.barrier = NWayBarrier.@new(nWorkers / exh)
	cont init (x : unit ) =
	  do NWayBarrier.@ready(barrier / exh)
	  do NWayBarrier.@barrier(barrier / exh)
	  throw k(UNIT)
	fun spawn (i : int / exh : exh) : () =
	    if I32Gte(i, nWorkers)
	       then return()
	    else
		do apply spawnFn(i, init / exh)
		apply spawn(I32Add(i, 1) / exh)
	do apply spawn(0 / exh)
	do NWayBarrier.@barrier(barrier / exh)
        return()
      ;

    (* initiate a work group on all vprocs *)
      define inline @init-on-all-vprocs (group : group / exh : exh) : () =
	let fls : FLS.fls = FLS.@get()
	fun spawnFn (i : int, k : PT.fiber / exh : exh) : () =
	    let vp : vproc = VProc.@vproc-by-id(i)
            (* pin the worker to the ith vproc *)
	    let fls : FLS.fls = FLS.@pin-to(fls, i / exh)
	    VProcQueue.@enqueue-on-vproc(vp, fls, k)
	let nWorkers : int = VProc.@num-vprocs()
	do @spawn-n-workers(nWorkers, SELECT(GROUP_WORKER_INIT_OFF, group), spawnFn / exh)
        return()
      ;

    (* create a work group and make it ready to receive work *)
      define @group (workerInit : PT.fiber, 
		     spawnFn : fun(thread / exh -> unit),
                     removeFn : fun(thread / exh -> bool),
                     schedulerData : any
                    / exh : exh) : group =
        do @migrate-to-top-level-sched()
	let group : group = alloc(workerInit, spawnFn, removeFn, schedulerData)
        let group : group = promote(group)
        do @init-on-all-vprocs(group / exh)
	return(group)
      ;

    (* takes a fiber and a cancelable option, and returns a copy of the given fiber that can handle 
     * cancelation.
     *)
      define inline @wrap-cancelable (k : PT.fiber, c : Option.option / exh : exh) : PT.fiber =
         case c
	   of Option.NONE => return(k)
	    | Option.SOME(c : Cancelation.cancelable) =>
	      let k : PT.fiber = Cancelation.@wrap(c, k / exh)
              return(k)
          end
      ;

    (* construct an implicit thread. we obtain the environment by capturing the
     * implicit environment.
     * QUESTION: should the cancelation wrapping happen later in the @run operation?
     *) 
      define inline @capture (k : PT.fiber           (* to run the implicit thread *)
		             / exh : exh) : thread =
	let ite : ite = FLS.@get-ite( / exh)
        let c : Option.option = SELECT(ITE_CANCELABLE_OFF, ite)
        let k : PT.fiber = @wrap-cancelable(k, c / exh)
	let thread : thread = alloc(k, ite)
	return(thread)
      ;

    (* allocate an implicit thread *)
      define inline @alloc (ite : ite, k : PT.fiber / exh : exh) : thread =
	let thread : thread = alloc(k, ite)
	return(thread)
      ;

    (* create an implicit thread
     * QUESTION: should the cancelation wrapping happen later in the @run operation?
     *)
      define inline @thread (k : PT.fiber, 
			     c : Option.option           (* cancelable *)
                            / exh : exh) : thread =
      (* capture the work-group stack *)
	let ite : ite = FLS.@get-ite( / exh)
        let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	let ite' : ite = FLS.@ite(stk, c / exh)
        let c : Option.option = SELECT(ITE_CANCELABLE_OFF, ite)
        let k' : PT.fiber = @wrap-cancelable(k, c / exh)
	let thd : thread = alloc(k', ite')
	return(thd)
      ;

    (* run an implicit thread outside of a scheduler action *)
      define inline @run-out-of-scheduler (thd : thread / exh : exh) noreturn =
	do FLS.@set-ite(SELECT(ITE_OFF, thd) / exh)
	let k : PT.fiber = SELECT(FIBER_OFF, thd)
	throw k(UNIT)
      ;

    (* run an implicit thread from within a scheduler action *)
      define inline @run-in-scheduler (vp : vproc, sched : PT.sched_act, thd : thread / exh : exh) noreturn =
	do FLS.@set-ite(SELECT(ITE_OFF, thd) / exh)
	SchedulerAction.@run(vp, sched, SELECT(FIBER_OFF, thd))
      ;

    (* spawn the implicit thread on the work group at the top of the work-group stack *)
      define inline @spawn (thd : thread / exh : exh) : () =
        let group : group = @peek(UNIT / exh)
	let spawnFn : fun(thread / exh -> unit) = SELECT(GROUP_SPAWN_OFF, group)
        let _ : unit = apply spawnFn(thd / exh)
	return()
      ;

    (* remove a thread from the ready queue. returns true if the thread has migrated off the queue. *)
      define inline @remove-thread (thd : thread / exh : exh) : bool =
	let group : group = @peek(UNIT / exh)
	let removeFn : fun(thread / exh -> bool) = SELECT(GROUP_REMOVE_OFF, group)
	apply removeFn(thd / exh)
      ;

    (* get the scheduler data for current group *)
      define inline @get-scheduler-data (/ exh : exh) : any =
        let group : group = @peek(UNIT / exh)
	return(SELECT(GROUP_SCHEDULER_DATA_OFF, group))
      ;

    (* migrate the current fiber to the workgroup *)
      define inline @migrate-to-group (group : group / exh : exh) : () =
	cont k (x : unit) = return()
	let thd : thread = @thread (k, Option.NONE / exh)
	do @spawn(thd / exh)
	let _ : unit = SchedulerAction.@stop()
	return ()
      ;

    (* begin the dynamic scope of a group *)
      define inline @group-begin (group : group / exh : exh) : () =
	do @migrate-to-top-level-sched()
	do @init-ite(/ exh)
	do @push(group / exh)
        do @migrate-to-group(group / exh)
	return()
      ;

      define @group-begin-w (group : group / exh : exh) : unit =
	do @group-begin(group / exh)
	return(UNIT)
      ;

    (* end the dynamic scope of a group *)
      define @group-end (/ exh : exh) : () =
	let _ : Option.option = @pop(/ exh)
	do @migrate-to-top-level-sched()
	return()
      ;

    (* evaluate a thunk on a work group. *)
      define inline @run-with-group (group : group, f : fun(unit / exh -> any) / exh : exh) : any =
	do @group-begin(group / exh)
	let x : any = apply f (UNIT / exh)
	do @group-end(/ exh)
	return(x)
      ;

      define @run-with-group-w (arg : [group, fun(unit / exh -> any)] / exh : exh) : any =
        @run-with-group(#0(arg), #1(arg) / exh)
      ;

    )

    type group = _prim(group)

    val peek : unit -> group = _prim(@peek)
    val groupBegin : group -> unit = _prim(@group-begin-w)
    val runWithGroup : (group * (unit -> 'a)) -> 'a = _prim(@run-with-group-w)
    val currentGroup = peek

  end
