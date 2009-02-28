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

  (* evaluate a thunk with respect to a work group. *)
    val runWithGroup : group * (unit -> 'a) -> 'a
  (* get a reference to the group at the top of the work-group stack *)
    val currentGroup : unit -> group


    _prim(

    (* create a work group and make it ready to receive work *)
      define @group (workerInit : PT.fiber, spawnFn : fun(thread / exh -> unit) / exh : exh) : group;

    (* allociate an implicit thread *)
      define @alloc (ite : FLS.ite, k : PT.fiber / exh : exh) : thread;

    (* create an implicit thread *)
      define @thread (k : PT.fiber, 
		      c : Option.option           (* cancelable *)
                     / exh : exh) : thread;

    (* construct an implicit thread. we obtain the environment by capturing the
     * implicit environment.
     *) 
      define @capture (k : PT.fiber           (* fiber for implicit thread *)
		      / exh : exh) : thread;

    (* run an implicit thread outside of a scheduler action *)
      define @run-out-of-scheduler (thd : thread / exh : exh) noreturn;

    (* run an implicit thread from within a scheduler action *)
      define @run-in-scheduler (sched : PT.sched_act, thd : thread / exh : exh) noreturn;

    (* spawn an implicit thread on the work group at the top of the work-group stack *)
      define @spawn (thd : thread / exh : exh) : ();

    (* remove a thread from the ready queue *)
      define @remove-thread (thd : ImplicitThread.thread / exh : exh) : bool =

    (* get the scheduler data for current group *)
      define @get-scheduler-data (/ exh : exh) : any;

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

    (* access the work-group stack *)
      define @get-group-stk (x : unit / exh : exh) : PrimStk.stk =
        let iteOpt : Option.option = FLS.@find-ite(/ exh)
        case iteOpt
	 of Option.NONE =>
	    (* if there is no current ite, create a new one and set it as current *)
            let stk : PrimStk.stk = PrimStk.@new(UNIT / exh)
	    let ite : ite = FLS.@ite(stk, Option.NONE / exh)
	    do FLS.@set-ite(ite / exh)
            return(stk)
	  | Option.SOME (ite : ite) =>
	    let ite : ite = FLS.@get-ite(/ exh)
 	    return(SELECT(ITE_STACK_OFF, ite))
        end
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
	NWayBarrier.@barrier(barrier / exh)
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
	@spawn-n-workers(nWorkers, SELECT(GROUP_WORKER_INIT_OFF, group), spawnFn / exh)
      ;

    (* create a work group and make it ready to receive work *)
      define @group (workerInit : PT.fiber, 
		     spawnFn : fun(thread / exh -> unit),
                     removeFn : fun(thread / exh -> bool),
                     schedulerData : any
                    / exh : exh) : group =
	let group : group = alloc(workerInit, spawnFn, removeFn, schedulerData)
        let group : group = promote(group)
        do @init-on-all-vprocs(group / exh)
	return(group)
      ;

    (* takes a fiber and a cancelable option, and returns a copy of the given fiber that can handle 
     * cancelation.
     *)
      define @wrap-cancelable (k : PT.fiber, c : Option.option / exh : exh) : PT.fiber =
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
      define @capture (k : PT.fiber           (* to run the implicit thread *)
		      / exh : exh) : thread =
	let ite : ite = FLS.@get-ite( / exh)
        let c : Option.option = SELECT(ITE_CANCELABLE_OFF, ite)
        let k : PT.fiber = @wrap-cancelable(k, c / exh)
	let thread : thread = alloc(k, ite)
	return(thread)
      ;

    (* allocate an implicit thread *)
      define @alloc (ite : ite, k : PT.fiber / exh : exh) : thread =
	let thread : thread = alloc(k, ite)
	return(thread)
      ;

    (* create an implicit thread
     * QUESTION: should the cancelation wrapping happen later in the @run operation?
     *)
      define @thread (k : PT.fiber, 
		      c : Option.option           (* cancelable *)
                     / exh : exh) : thread =
      (* capture the work-group stack *)
	let ite : ite = FLS.@get-ite( / exh)
        let stk : PrimStk.stk = SELECT(ITE_STACK_OFF, ite)
        let stk : PrimStk.stk = PrimStk.@copy(stk / exh)
	let ite' : ite = FLS.@ite(stk, c / exh)
        let c : Option.option = SELECT(ITE_CANCELABLE_OFF, ite)
        let k' : PT.fiber = @wrap-cancelable(k, c / exh)
	let thd : thread = alloc(k', ite')
	return(thd)
      ;

    (* run an implicit thread outside of a scheduler action *)
      define @run-out-of-scheduler (thd : thread / exh : exh) noreturn =
	do FLS.@set-ite(SELECT(ITE_OFF, thd) / exh)
	let k : PT.fiber = SELECT(FIBER_OFF, thd)
	throw k(UNIT)
      ;

    (* run an implicit thread from within a scheduler action *)
      define @run-in-scheduler (sched : PT.sched_act, thd : thread / exh : exh) noreturn =
	do FLS.@set-ite(SELECT(ITE_OFF, thd) / exh)
	SchedulerAction.@run(host_vproc, sched, SELECT(FIBER_OFF, thd))
      ;

    (* return the top of the group stack.
     * NOTE: an exception is raised if the stack is empty
     *)
      define @get-group-stack-top (/ exh : exh) : group =
        let stk : PrimStk.stk = @get-group-stk(UNIT / exh)
        let group : Option.option = PrimStk.@peek(stk / exh)
        case group
	 of Option.NONE =>
	    let e : exn = Fail(@"ImplicitThread.@get-stack-top: empty work-group stack")
            throw exh(e)
	  | Option.SOME(group : group) =>
	    return(group)
        end
      ;

    (* spawn the implicit thread on the work group at the top of the work-group stack *)
      define @spawn (thd : thread / exh : exh) : () =
        let group : group = @get-group-stack-top(/ exh)
	let spawnFn : fun(thread / exh -> unit) = SELECT(GROUP_SPAWN_OFF, group)
        let _ : unit = apply spawnFn(thd / exh)
	return()
      ;

    (* remove a thread from the ready queue. returns true iff the thread had been migrated off the queue. *)
      define @remove-thread (thd : thread / exh : exh) : bool =
	let group : group = @get-group-stack-top(/ exh)
	let removeFn : fun(thread / exh -> bool) = SELECT(GROUP_REMOVE_OFF, group)
	apply removeFn(thd / exh)
      ;

    (* get the scheduler data for current group *)
      define @get-scheduler-data (/ exh : exh) : any =
        let group : group = @get-group-stack-top(/ exh)
	return(SELECT(GROUP_SCHEDULER_DATA_OFF, group))
      ;

    )

    type group = _prim(group)

    val getGroupStk : unit -> group PrimStk.stk = _prim(@get-group-stk)

    fun outOfScope group = () (* TODO *)

  (* evaluate a thunk with respect to a work group. *)
    fun runWithGroup (group, f) = let
	  val groupStk = getGroupStk()
	  val () = PrimStk.push(groupStk, group)
	  val x = f()
          in
	    outOfScope group;
	    PrimStk.pop groupStk;
	    x
	  end

  (* get a reference to the group at the top of the work-group stack *)
    fun currentGroup () = (
	  case PrimStk.peek(getGroupStk())
	   of Option.NONE => (raise Fail "currentGroup: empty group stack")
	    | Option.SOME group => group
          (* end case *))

  end
