(* hlop-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * TODO: eventually, this information should be loaded from a file.
 *
 * QUESTION: what is the relationship between this file and bom/manticore-ops.sml?
 *)

structure HLOpEnv : sig

(*
    val qItemAlloc : HLOp.hlop  	(* allocate a queue item *)
    val qEnqueue : HLOp.hlop		(* insert an item [nonatomic] *)
    val qDequeue : HLOp.hlop 		(* remove an item [nonatomic] *)
    val qEmpty : HLOp.hlop		(* return true if queue is empty [nonatomic] *)

  (* concurrent queue operations *)
    val atomicQEnqueue : HLOp.hlop	(* insert an item [atomic] *)
    val atomicQDequeue : HLOp.hlop	(* remove an item [atomic] *)
*)

  (* high-level operations used to implement Manticore language constructs *)
    val spawnOp : HLOp.hlop
    val threadExitOp : HLOp.hlop
    val iVarOp : HLOp.hlop
    val iGetOp : HLOp.hlop
    val iPutOp : HLOp.hlop

  (* Basis functions *)
    val listAppendOp : HLOp.hlop
    val stringConcatOp : HLOp.hlop
    val stringLitOp : HLOp.hlop
    val itosOp : HLOp.hlop
    val ltosOp : HLOp.hlop
    val ftosOp : HLOp.hlop
    val dtosOp : HLOp.hlop
    val printOp : HLOp.hlop

  (* scheduler operations *)
    val runOp : HLOp.hlop
    val forwardOp : HLOp.hlop
    val dequeueOp : HLOp.hlop
    val enqueueOp : HLOp.hlop

  (* generate a high-level operator for a custom scheduler startup *)
    val schedulerStartupOp : string -> HLOp.hlop

  (* events *)
    val wrapOp : HLOp.hlop
    val chooseOp : HLOp.hlop
    val alwaysOp : HLOp.hlop
    val syncOp : HLOp.hlop

  (* work queues *)
    val newWorkQueueOp : HLOp.hlop
    val getWork1AllOp  : HLOp.hlop

  (* futures *)
    val futureOp : HLOp.hlop
    val touchOp  : HLOp.hlop
    val cancelOp : HLOp.hlop

    val future1Op : HLOp.hlop
    val touch1Op  : HLOp.hlop
    val cancel1Op : HLOp.hlop

    val define : HLOp.hlop -> unit
    val find : Atom.atom -> HLOp.hlop option

  end = struct

    structure H = HLOp
    structure BTy = BOMTy
    structure Basis = BOMBasis

    type env = HLOp.hlop AtomTable.hash_table

  (* some standard parameter types *)
    val unitTy = BTy.unitTy
    val boolTy = BTy.boolTy
    val exnTy = BTy.exnTy
    val exhTy = BTy.exhTy
    val tidTy = BTy.tidTy
    val fiberTy = BTy.fiberTy

    val signalTy = Basis.signalTy
    val sigActTy = BTy.T_Cont[signalTy]

    val vprocTy = BTy.T_VProc
    val rawIntTy = BTy.T_Raw BTy.T_Int
    val listTy = Basis.listTy
    val intTy = BOMBasis.intTy
    val longTy = BOMBasis.longTy
    val floatTy = BOMBasis.floatTy
    val doubleTy = BOMBasis.doubleTy
    val stringTy = BOMBasis.stringTy

    val evtTy = BOMBasis.evtTy

    val workQueueTy = BTy.T_Any

    val ivarTy = BTy.T_Tuple(true, [listTy, BTy.T_Any, rawIntTy])
    val thunkTy = BTy.thunkTy
    val futureTy = BTy.futureTy

    fun pairTy (ty1, ty2) = BTy.T_Tuple(false, [ty1, ty2])

  (* new : string * BTy.ty list * BTy.ty list * H.attributes list -> H.hlop *)
    fun new (name, params, res, attrs) =
	  H.new (Atom.atom name, 
		 {params= List.map HLOp.PARAM params, exh=[], results=res}, 
		 attrs)

  (* newWithExh : string * BTy.ty list * BTy.ty list * H.attributes list -> H.hlop *)
    fun newWithExh (name, params, res, attrs) =
	  H.new (Atom.atom name,
		 {params = map H.PARAM params, exh = [exhTy], results = res},
		 attrs)

  (* high-level operations used to implement Manticore language constructs *)
    val listAppendOp = newWithExh("list-append", [pairTy(listTy, listTy)], [listTy], [])
    val stringConcatOp = newWithExh("string-concat2", [pairTy(stringTy, stringTy)], [stringTy], [])
    val stringLitOp = new("string-lit", [BTy.T_Any, rawIntTy], [stringTy], [])

  (* high-level operations used to implement Manticore concurrency constructs *)
    val spawnOp = newWithExh ("spawn", [BTy.T_Fun([], [exhTy], [])], [tidTy], [])
    val threadExitOp = newWithExh ("thread-exit", [], [], [H.NORETURN])
    val iVarOp = newWithExh ("iVar", [], [ivarTy], [])
    val iGetOp = newWithExh ("iGet", [ivarTy], [BTy.T_Any], [])
    val iPutOp = newWithExh ("iPut", [ivarTy, BTy.T_Any], [], [])

  (* Basis functions *)
    val itosOp = newWithExh ("itos", [intTy], [stringTy], [])
    val ltosOp = newWithExh ("ltos", [longTy], [stringTy], [])
    val ftosOp = newWithExh ("ftos", [floatTy], [stringTy], [])
    val dtosOp = newWithExh ("dtos", [doubleTy], [stringTy], [])
    val printOp = newWithExh ("print", [stringTy], [unitTy], [])

  (* scheduler operations *)
    val runOp = newWithExh ("run", [vprocTy, sigActTy, tidTy, fiberTy], [], [H.NORETURN])
    val forwardOp = newWithExh ("forward", [vprocTy, signalTy], [], [H.NORETURN])
    val dequeueOp = newWithExh ("dequeue", [vprocTy], [Basis.rdyqItemTy], [])
    val enqueueOp = newWithExh ("enqueue", [vprocTy, tidTy, fiberTy], [], [])

  (* generate a high-level operator for a scheduler startup *)
    fun schedulerStartupOp name = newWithExh (name ^ "-startup", [], [], [])

  (* events *)
    val alwaysOp = newWithExh ("event-always", [BTy.T_Any], [evtTy], [])
    val chooseOp = newWithExh ("event-choose", [pairTy(evtTy, evtTy)], [evtTy], [])
    val wrapOp = newWithExh (
	  "event-wrap",
	  [pairTy(evtTy, BTy.T_Fun([BTy.T_Any], [exhTy], [BTy.T_Any]))],
	  [evtTy],
	  [])
    val syncOp = newWithExh ("event-sync", [evtTy], [BTy.T_Any], [])

  (* work queue operations *)
    val newWorkQueueOp = newWithExh ("new-work-queue", [unitTy], [workQueueTy], [])
    val getWork1AllOp = newWithExh ("get-work1-all", [workQueueTy], [unitTy], [])

  (* futures *)
    (* FIXME set these up with work queues too *)
    val futureOp = newWithExh ("future", [thunkTy], [futureTy], [])
    val touchOp  = newWithExh ("touch",  [futureTy], [BTy.T_Any], [])
    val cancelOp = newWithExh ("cancel", [futureTy], [], [])

    val future1Op = newWithExh (
	  "future1", 
	  [pairTy (workQueueTy, thunkTy)],
	  [futureTy], 
	  [])

    val touch1Op  = newWithExh (
	  "touch1",  
	  [pairTy (workQueueTy, futureTy)], 
	  [BTy.T_Any], 
	  [])

    val cancel1Op = newWithExh (
	  "cancel1", 
	  [pairTy (workQueueTy, futureTy)],
	  [], 
	  [])
		    
    fun mkTbl nameOf bindings = let
	  val tbl = AtomTable.mkTable (List.length bindings, Fail "table")
	  fun ins v = AtomTable.insert tbl (nameOf v, v)
	  in
	    List.app ins bindings;
	    AtomTable.find tbl
	  end 

  (* HLOp table *)
    val tbl : HLOp.hlop AtomTable.hash_table = AtomTable.mkTable (128, Fail "HLOp table")

    val find : Atom.atom -> HLOp.hlop option = AtomTable.find tbl
    fun define hlop = AtomTable.insert tbl (HLOp.name hlop, hlop)

  (* insert predefined HLOps *)
    val _ = List.app define [
		spawnOp,
		threadExitOp,
		iVarOp,
		iGetOp,
		iPutOp,
		dequeueOp,
		enqueueOp,
		forwardOp,
		runOp,
		newWorkQueueOp,
		getWork1AllOp,
		futureOp,
		touchOp,
		cancelOp,
		future1Op,
		touch1Op,
		cancel1Op
	      ]

  end
