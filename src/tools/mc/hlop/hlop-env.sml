(* hlop-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * TODO: eventually, this information should be loaded from a file.
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
    val listAppendOp : HLOp.hlop
    val spawnOp : HLOp.hlop
    val threadExitOp : HLOp.hlop
    val iVarOp : HLOp.hlop
    val iGetOp : HLOp.hlop
    val iPutOp : HLOp.hlop

  (* scheduler operations *)
    val runOp : HLOp.hlop
    val forwardOp : HLOp.hlop
    val dequeueOp : HLOp.hlop
    val enqueueOp : HLOp.hlop

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

  (* some standard parameter types *)
    val unitTy = BTy.unitTy
    val boolTy = BTy.boolTy
    val exnTy = BTy.exnTy
    val exhTy = BTy.exhTy
    val tidTy = BTy.tidTy
    val fiberTy = BTy.fiberTy

    val signalTy = BTy.T_TyCon Basis.signalTyc
    val sigActTy = BTy.T_Cont[signalTy]

    val vprocTy = BTy.T_Any	(* FIXME *)
    val listTy = BTy.T_TyCon Basis.listTyc

    val ivarTy = BTy.T_Tuple(true, [listTy, BTy.T_Any, BTy.T_Raw BTy.T_Int])
    val thunkTy = BTy.thunkTy
    val futureTy = BTy.futureTy

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
    val listAppendOp = new("list-append", [listTy, listTy], [], [])
    val spawnOp = new("spawn", [BTy.T_Fun([], [exhTy], [])], [tidTy], [])
    val threadExitOp = new("thread-exit", [], [], [H.NORETURN])
    val iVarOp = new("iVar", [], [ivarTy], [])
    val iGetOp = new("iGet", [ivarTy], [BTy.T_Any], [])
    val iPutOp = new("iPut", [ivarTy, BTy.T_Any], [], [])

  (* scheduler operations *)
    val defaultSchedulerStartupOp = new("default-scheduler-startup", [], [], [])
    val schedulerStartupOp = new("scheduler-startup", [sigActTy], [], [])
    val runOp = new("run", [vprocTy, sigActTy, tidTy, fiberTy], [], [H.NORETURN])
    val forwardOp = new("forward", [vprocTy, signalTy], [], [H.NORETURN])
    val dequeueOp = new("dequeue", [vprocTy], [Basis.rdyqItemTy], [])
    val enqueueOp = new("enqueue", [vprocTy, tidTy, fiberTy], [], [])

  (* futures *)
    val futureOp = newWithExh ("future", [thunkTy], [futureTy], [])
    val touchOp  = newWithExh ("touch",  [futureTy], [BTy.T_Any], [])
    val cancelOp = newWithExh ("cancel", [futureTy], [], [])

    val future1Op = newWithExh ("future1", [thunkTy], [futureTy], [])
    val touch1Op  = newWithExh ("touch1",  [futureTy], [BTy.T_Any], [])
    val cancel1Op = newWithExh ("cancel1", [futureTy], [], [])
		    
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
                schedulerStartupOp,
                defaultSchedulerStartupOp
	      ]

  end
