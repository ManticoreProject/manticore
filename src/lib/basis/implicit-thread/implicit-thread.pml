(* implicit-thread.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Underlying support for implicitly-generated threads.
 *)

structure ImplicitThread (* :
  sig

    type work_group

    val runOnWorkGroup : work_group * (unit -> 'a) -> 'a
    val currentWorkGroup : unit -> work_group
    val defaultWorkGroupBegin : work_group -> unit

    _prim (

    (* scheduler-specific state provided by the scheduler *)
      typedef scheduler_state = any;

      typedef thread;
      typedef worker;
      typedef work_group;

    (** Implicit thread creation **)

      define inline @new-thread (k : PT.fiber / exh : exh) : thread;
      define inline @new-cancelable-thread (k : PT.fiber, c : Cancelation.cancelable / exh : exh) : thread;
    (* creates a new thread by pairing the fiber with the current implicit-thread environment *)
      define inline @capture (k : PT.fiber / exh : exh) : thread;

    (** Work group management **)

      define @new-work-group (workGroupId : Word64.word,
			      spawnFn : fun (thread / exh -> unit),
			      removeFn : fun (thread / exh -> unit),
			      schedulerState : scheduler_state,
			      terminated : ![bool]
			    / exh : exh) : work_group;
    (* spawn the worker initialized by initWorker (using the given fls) on the vproc dst *)
      define inline @spawn-worker (group : work_group, 
				   dst : vproc, 
				   workerFLS : FLS.fls, 
				   initWorker : cont (worker) 
				 / exh : exh) : worker;
      define inline @work-group-id (group : work_group) : Word64.word;
    (* return the work group at the  top of the work-group stack. an exception is raised
     * if the stack is empty. *)
      define inline @current-work-group (_ : unit / exh : exh) : work_group;
    (* suspend / resume the execution of all workers local to the given vproc *)
      define @suspend-vproc (workGroup : work_group, vp : vproc / exh : exh) : ();
      define @resume-vproc (workGroup : work_group, vp : vproc / exh : exh) : ();
    (* place the given implicit thread on the ready queue of the current work group *)
      define inline @spawn-thread (thd : thread / exh : exh) : thread;
    (* remove the given thread from the ready queue, supposing the thread is already on the ready queue. if 
     * the return value is true, then the thread was both on the ready queue and successfully removed by
     * the operation. otherwise, the return value must be false. note that this prescribed behavior provides
     * some latitude to the scheduler, which may, for example, choose to always return false. *)
      define inline @remove-thread (thd : thread / exh : exh) : bool;
      define @terminated-flag () : ![bool];
    (* terminate the work group (the workers in the group cease to execute new work) *)
      define @terminate-work-group (workGroup : work_group) : ();

    (** Operations to be used by the scheduler loop **)

      define inline @throw-to (thd : thread / exh : exh) noreturn;
      define inline @run-from-atomic (vp : vproc, act : PT.sched_act, thd : thread / exh : exh) noreturn;
      define inline @get-scheduler-state (/ exh : exh) : scheduler_state;

    )

  end *) = struct

(* offsets into tuples *)
#define WORK_GROUP_UID_OFF                      0
#define WORK_GROUP_SPAWN_FUN_OFF                1
#define WORK_GROUP_REMOVE_FUN_OFF               2
#define WORK_GROUP_SCHEDULER_STATE_OFF          3
#define WORK_GROUP_SUSPEND_OFF                  4
#define WORK_GROUP_TERMINATED_OFF               5

#define ITE_STACK_OFF         0
#define ITE_CANCELABLE_OFF    1

#define THREAD_FIBER_OFF   0
#define THREAD_ITE_OFF     1

      structure PT = PrimTypes
      structure Arr = Array64

      _primcode (

      (* implicit thread = suspended fiber + environment *)
	typedef thread = [ PT.fiber, FLS.ite ];
      (* scheduler-specific state provided by the scheduler *)
	typedef scheduler_state = any;

        typedef worker = Word64.word;

	typedef work_group = 
		 [
		   Word64.word,                   (* unique id *)
		   fun (thread / exh -> unit),    (* spawn function *)
		   fun (thread / exh -> bool),    (* thread removal function *)
		   scheduler_state,               (* scheduler-specific state provided by the scheduler *)
		   Arr.array,                     (* the ith entry is true, if the work group is suspended
						   * on the ith vproc *)
		   ![bool]                        (* true, when the work group has terminated *)
		 ];

      )

    type work_group = _prim (work_group)

  (** Implicit thread creation **)

    _primcode (

      define (* inline *) @new-thread (k : PT.fiber / exh : exh) : thread =
	  let ite : FLS.ite = FLS.@get-ite ( / exh)
          let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	  let ite' : FLS.ite = FLS.@ite (stk, Option.NONE / exh)
	  return (alloc (k, ite'))
	;

      define (* inline *) @new-cancelable-thread (k : PT.fiber, c : Cancelation.cancelable / exh : exh) : thread =
	  let ite : FLS.ite = FLS.@get-ite ( / exh)
          let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	  let ite' : FLS.ite = FLS.@ite (stk, Option.SOME(c) / exh)
	  let k : PT.fiber = Cancelation.@wrap (c, k / exh)
	  return (alloc (k, ite'))
	;

    (* pairs the fiber with the current implicit-thread environment to create a new thread *)
      define (* inline *) @capture (k : PT.fiber / exh : exh) : thread =
	  let ite : FLS.ite = FLS.@get-ite ( / exh)
	  let c : Option.option = SELECT(ITE_CANCELABLE_OFF, ite)
	  let k : PT.fiber = case c
			      of Option.NONE => 
				 return (k)
			       | Option.SOME(c : Cancelation.cancelable) => 
				 let k : PT.fiber = Cancelation.@wrap (c, k / exh)
				 return (k)
			     end
	  return (alloc (k, ite))
	;

    )

  (** Work group management **)

    local

      _primcode (

	(* seed the implicit-threading environment *)
	  define (* inline *) @seed-ite ( / exh : exh) : () =
	      let iteOpt : Option.option = FLS.@find-ite (/ exh)
	      case iteOpt
	       of Option.NONE =>
		  (* if there is no current ite, create a new one and set it as current *)
		  let ite : FLS.ite = FLS.@ite (List.nil, Option.NONE / exh)
		  do FLS.@set-ite (ite / exh)
		  return ()
		| Option.SOME (ite : FLS.ite) =>
		  return ()
	      end
	    ;

	(* push the given work group on the top of the work-group stack *)
	  define (* inline *) @push-work-group (group : work_group / exh : exh) : () =
	      let ite : FLS.ite = FLS.@get-ite ( / exh)
	      let newStk : List.list = List.CONS(group, SELECT(ITE_STACK_OFF, ite))
	      do FLS.@set-ite (alloc (newStk, SELECT(ITE_CANCELABLE_OFF, ite)) / exh)
	      return ()
	    ;

	(* pop from the top of the work-group stack *)
	  define (* inline *) @pop-work-group ( / exh : exh) : Option.option =
	      let ite : FLS.ite = FLS.@get-ite ( / exh)
	      let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	      case stk
	       of List.nil => 
		  return (Option.NONE)
		| List.CONS(x : any, stk : List.list) => 
		  do FLS.@set-ite (alloc(stk, SELECT(ITE_CANCELABLE_OFF, ite)) / exh)
		  return (Option.SOME(x))
	      end
	    ;

	(* migrate the current thread to the top-level scheduler *)
	  define (* inline *) @migrate-to-top-level-sched (/ exh : exh) : () = 
	      cont k (_ : unit) = return ()
	      let fls : FLS.fls = FLS.@get ()
	      do VProcQueue.@enqueue (fls, k)
	      let _ : unit = SchedulerAction.@stop ()
	      return ()
	    ;

      )

    in

    _primcode (

	define @new-work-group (workGroupId : Word64.word,
				spawnFn : fun(thread / exh -> unit),
				removeFn : fun(thread / exh -> bool),
				schedulerState : scheduler_state,
				terminated : ![bool]
			      / exh : exh) : work_group =
	    do @migrate-to-top-level-sched (/ exh)	    
            let nVProcs : int = VProc.@num-vprocs ()
            let suspendResumeArr : Arr.array = Arr.@array (nVProcs, false / exh)
            let group : work_group = promote (alloc (workGroupId,
						     spawnFn, 
						     removeFn, 
						     schedulerState,
						     suspendResumeArr,
						     terminated))
	    return (group)
	  ;

    (* spawn the worker initialized by initWorker (using the given fls) on the vproc dst *)
      define inline @spawn-worker (group : work_group, 
				   dst : vproc, 
				   workerFLS : FLS.fls, 
				   initWorker : cont (worker)
				 / exh : exh) : worker =
	  let worker : Word64.word = UID.@new (/ exh)
          let i : int = VProc.@vproc-id (dst)
	  let workerFLS' : FLS.fls = FLS.@pin-to (workerFLS, i / exh)
	  cont initWorker' (_ : unit) = throw initWorker (worker)
	  do VProcQueue.@enqueue-on-vproc (dst, workerFLS', initWorker')
	  return (worker)
        ;

      define (* inline *) @work-group-id (group : work_group) : Word64.word =
	  return (SELECT(WORK_GROUP_UID_OFF, group))
	;

    (* return the work group at the  top of the work-group stack. an exception is raised
     * if the stack is empty. *)
      define (* inline *) @current-work-group (_ : unit / exh : exh) : work_group =
	  let ite : FLS.ite = FLS.@get-ite (/ exh)
	  let stk : List.list = SELECT(ITE_STACK_OFF, ite)
	  case stk
	   of List.nil => 
	      let e : exn = Fail(@"ImplicitThread.@current-work-group: empty work-group stack")
	      throw exh (e)
	    | List.CONS(group : work_group, stk : List.list) =>
	      return (group)
	  end
	;

    (* suspend / resume the execution of all workers local to the given vproc *)
      define @suspend-vproc (workGroup : work_group, vp : vproc / exh : exh) : () =
	  let vpId : int = VProc.@vproc-id (vp)
	  do Arr.@update (SELECT(WORK_GROUP_SUSPEND_OFF, workGroup), vpId, true / exh)
	  return ()
	;

      define @resume-vproc (workGroup : work_group, vp : vproc / exh : exh) : () =
	  let vpId : int = VProc.@vproc-id (vp)
	  do Arr.@update (SELECT(WORK_GROUP_SUSPEND_OFF, workGroup), vpId, false / exh)
	  return ()
	;

    (* place the given implicit thread on the ready queue of the current work group *)
      define (* inline *) @spawn-thread (thd : thread / exh : exh) : () =
	  let group : work_group = @current-work-group (UNIT / exh)
	  let spawnFn : fun(thread / exh -> unit) = SELECT(WORK_GROUP_SPAWN_FUN_OFF, group)
	  let _ : unit = apply spawnFn (thd / exh)
	  return ()
	;

    (* remove the given thread from the ready queue, supposing the thread is already on the ready queue. if 
     * the return value is true, then the thread was both on the ready queue and successfully removed by
     * the operation. otherwise, the return value must be false. note that this prescribed behavior provides
     * some latitude to the scheduler, which may, for example, choose to always return false. *)
      define (* inline *) @remove-thread (thd : thread / exh : exh) : bool =
	  let group : work_group = @current-work-group (UNIT / exh)
	  let removeFn : fun(thread / exh -> bool) = SELECT(WORK_GROUP_REMOVE_FUN_OFF, group)
	  apply removeFn (thd / exh)
	;

      define @terminated-flag () : ![bool] =
	  let terminated : ![bool] = alloc (false)
	  let terminated : ![bool] = promote (terminated)
	  return (terminated)
	;

    (* terminate the work group (the workers in the group cease to execute new work) *)
      define @terminate-work-group (workGroup : work_group) : () =
	  do #0(SELECT(WORK_GROUP_TERMINATED_OFF, workGroup)) := true
	  return ()
	;

    (* begin the dynamic scope of a work group *)
      define (* inline *) @work-group-begin (group : work_group / exh : exh) : () =
	  do @migrate-to-top-level-sched (/ exh)
	  do @seed-ite (/ exh)
	  do @push-work-group (group / exh)
	(* assign the continuation of the current thread to the first thread on the work group's 
	 * ready queue *)
	  cont k (x : unit) = return ()
	  let thd : thread = @new-thread (k / exh)
	  do @spawn-thread (thd / exh)
	  let _ : unit = SchedulerAction.@stop ()
	  return ()
	;

    (* end the dynamic scope of a work group *)
      define @work-group-end (/ exh : exh) : () =
	  let _ : Option.option = @pop-work-group (/ exh)
	  do @migrate-to-top-level-sched (/ exh)
	  return ()
	;

      define (* inline *) @run-on-work-group (group : work_group, f : fun(unit / exh -> any) / exh : exh) : any =
	  do @work-group-begin (group / exh)
	  let x : any = apply f (UNIT / exh)
	  do @work-group-end (/ exh)
	  return (x)
	;

      define @run-on-work-group-w (arg : [work_group, fun(unit / exh -> any)] / exh : exh) : any =
	  @run-on-work-group(#0(arg), #1(arg) / exh)
	;

      define @default-work-group-begin (defaultGroup : work_group / exh : exh) : unit =
	  do @seed-ite (/ exh)
	  do @push-work-group (defaultGroup / exh)
	  return (UNIT)
	;

    )

    val runOnWorkGroup : work_group * (unit -> 'a) -> 'a = _prim (@run-on-work-group-w)
    val defaultWorkGroupBegin : work_group -> unit = _prim (@default-work-group-begin)

    end (* local *)

  (** Operations to be used by the scheduler loop **)

    _primcode (

      define (* inline *) @throw-to (thd : thread / exh : exh) noreturn =
	  do FLS.@set-ite (SELECT(THREAD_ITE_OFF, thd) / exh)
	  let k : PT.fiber = SELECT(THREAD_FIBER_OFF, thd)
	  throw k (UNIT)
	;

      define (* inline *) @run-from-atomic (vp : vproc, act : PT.sched_act, thd : thread / exh : exh) noreturn =
	  do FLS.@set-ite (SELECT(THREAD_ITE_OFF, thd) / exh)
	  SchedulerAction.@run (vp, act, SELECT(THREAD_FIBER_OFF, thd))
	;

      define (* inline *) @get-scheduler-state (/ exh : exh) : scheduler_state =
	  let group : work_group = @current-work-group (UNIT / exh)
	  return (SELECT(WORK_GROUP_SCHEDULER_STATE_OFF, group))
        ;

    )

  end
