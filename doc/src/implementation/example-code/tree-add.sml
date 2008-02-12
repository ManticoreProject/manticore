signature RTM =
  sig
    
    type fiber = unit SMLofNJ.Cont.cont
    datatype signal = STOP | PREEMPT of fiber
    type fgs
    type sched_action = signal SMLofNJ.Cont.cont

    val newFgs : unit -> fgs
    val run : (sched_action * fgs * fiber) -> 'a
    val forward : signal -> 'a    

    val testAndSet : bool ref -> bool

  end

signature LAZY_TASK_CREATION =
  sig

    val push : (unit -> 'a) -> unit
    val pop : unit -> 'a

  end

signature IVAR =
  sig
    
    type 'a ivar

    val new : unit -> 'a ivar
    val get : 'a ivar -> 'a
    val put : ('a ivar * 'a) -> unit

  end

structure TreeAddEx =
  struct

    fun future x = x

    datatype tree = EMPTY
		  | NODE of {i : int, left : tree, right : tree}

    fun treeAdd EMPTY = 0
      | treeAdd (NODE {i, left, right}) =
	(treeAdd(right) + i) + future(treeAdd(left))

  end (* TreeAddEx *)

functor TreeAddRTMExFn (
    structure RTM : RTM
    structure LazyTaskCreation : LAZY_TASK_CREATION
    structure IVar : IVAR
  ) = struct

    datatype tree = EMPTY
		  | NODE of {i : int, left : tree, right : tree}

    fun treeAdd EMPTY = 0
      | treeAdd (NODE {i, left, right}) = let
	val done = ref false
	val ivar = IVar.new ()
	fun ctx () = (
            if (RTM.testAndSet(done))
               then ()
               else ignore((treeAdd(right) + i) + IVar.get(ivar));
            LazyTaskCreation.pop())	      
        in
           LazyTaskCreation.push(ctx);
	   IVar.put(ivar, treeAdd(left));
	   LazyTaskCreation.pop()
        end

  end (* TreeAddRTMExFn *)
