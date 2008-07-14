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
    val listMapOp : HLOp.hlop
    val listRevOp : HLOp.hlop
    val listNthOp : HLOp.hlop
    val failOp : HLOp.hlop
    val stringConcatOp : HLOp.hlop
    val stringConcatListOp : HLOp.hlop
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

  (* channels *)
    val channelOp : HLOp.hlop
    val recvOp : HLOp.hlop
    val recvEvtOp : HLOp.hlop
    val sendOp : HLOp.hlop

  (* work queues *)
    val newWorkQueueOp : HLOp.hlop
    val getWork1AllOp  : HLOp.hlop

  (* futures *)
    val futureOp : HLOp.hlop
    val touchOp  : HLOp.hlop
    val cancelOp : HLOp.hlop

    val future1SpawnOp  : HLOp.hlop
    val future1TouchOp  : HLOp.hlop
    val future1CancelOp : HLOp.hlop

  (* parrays (ropes) *)
    val ropeSubOp       : HLOp.hlop
    val ropeLengthIntOp : HLOp.hlop
    val ropeFromRangeOp : HLOp.hlop

  (* some hlops, not in the surface language, for use in rope maps *)
    val extractShortestRopeOp : HLOp.hlop
    val curriedRopeSublistOp : HLOp.hlop
    val insertAtOp : HLOp.hlop
    
  (* extras *)
    val newImageOp	: HLOp.hlop
    val updateImage3fOp	: HLOp.hlop
    val updateImage3dOp	: HLOp.hlop
    val outputImageOp	: HLOp.hlop
    val freeImageOp	: HLOp.hlop
    val getNumProcs	: HLOp.hlop
    val getNumVProcs    : HLOp.hlop

    val define : HLOp.hlop -> unit
    val find : Atom.atom -> HLOp.hlop option

    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	pmlImports : (BOM.var * BOM.var) list,  (* imports from PML *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

    val addDefs : hlop_def list -> unit
    val findDef : HLOp.hlop -> hlop_def option
    val listHLOps : unit -> BOM.lambda list

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
    val anyTy = BTy.T_Any

    val signalTy = Basis.signalTy
    val sigActTy = BTy.T_Cont[signalTy]

    val vprocTy = BTy.T_VProc
    val rawIntTy = BTy.T_Raw BTy.T_Int
    val listTy = Basis.listTy
    val intTy = Basis.intTy
    val longTy = Basis.longTy
    val floatTy = Basis.floatTy
    val doubleTy = Basis.doubleTy
    val stringTy = Basis.stringTy
    val ropeTy = Basis.ropeTy

    val evtTy = Basis.evtTy

    val workQueueTy = BTy.T_Any

    val ivarTy = BTy.T_Tuple(true, [listTy, BTy.T_Any, rawIntTy])
    val thunkTy = BTy.thunkTy
    val futureTy = BTy.futureTy

    fun pairTy (ty1, ty2) = BTy.T_Tuple(false, [ty1, ty2])

  (* new : string * BTy.ty list * BTy.ty list * H.attributes list -> H.hlop *)
    fun new (name, params, res, attrs) =
	  H.new (Atom.atom name, 
		 {params = List.map HLOp.PARAM params, exh=[], results=res}, 
		 attrs)

  (* newWithExh : string * BTy.ty list * BTy.ty list * H.attributes list -> H.hlop *)
    fun newWithExh (name, params, res, attrs) =
	  H.new (Atom.atom name,
		 {params = List.map H.PARAM params, exh = [exhTy], results = res},
		 attrs)

  (* high-level operations used to implement Manticore language constructs *)
    val listAppendOp = newWithExh("list-append", [pairTy(listTy, listTy)], [listTy], [])

    val listMapOp = 
	let val fnTy = BTy.T_Fun ([anyTy], [exhTy], [anyTy])
	in
	    newWithExh("list-map", [pairTy(fnTy, listTy)], [listTy], [])
	end

    val listRevOp = newWithExh("list-rev", [listTy], [listTy], [])
    val listNthOp = newWithExh("list-nth", [pairTy(listTy, intTy)], [anyTy], [])

    val failOp = newWithExh("fail", [stringTy], [anyTy], [])

    val stringConcatOp = newWithExh("string-concat2", [pairTy(stringTy, stringTy)], [stringTy], [])
    val stringConcatListOp = newWithExh("string-concat-list", [listTy], [stringTy], [])
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

    val channelOp = newWithExh ("chan-new", [unitTy], [Basis.chanTy], [])
    val recvOp = newWithExh ("chan-recv", [Basis.chanTy], [BTy.T_Any], [])
    val recvEvtOp = newWithExh ("chan-recv-evt", [Basis.chanTy], [evtTy], [])
    val sendOp = newWithExh ("chan-send", [pairTy(Basis.chanTy, BTy.T_Any)], [unitTy], [])

  (* work queue operations *)
    val newWorkQueueOp = newWithExh ("new-work-queue", [unitTy], [workQueueTy], [])
    val getWork1AllOp = newWithExh ("get-work1-all", [workQueueTy], [unitTy], [])

  (* futures *)
    (* FIXME set these up with work queues too *)
    val futureOp = newWithExh ("future", [thunkTy], [futureTy], [])
    val touchOp  = newWithExh ("touch",  [futureTy], [BTy.T_Any], [])
    val cancelOp = newWithExh ("cancel", [futureTy], [], [])

    val future1SpawnOp = 
	  newWithExh ("future1-spawn", [thunkTy], [futureTy], [])

    val future1TouchOp  = 
	  newWithExh ("future1-touch", [futureTy], [BTy.T_Any], [])

    val future1CancelOp = 
	  newWithExh ("future1-cancel", [futureTy], [], [])

    val ropeSubOp = 
	  newWithExh ("rope-sub", [pairTy (Basis.ropeTy, intTy)], [BTy.T_Any], [])
		
    val ropeLengthIntOp =
	  newWithExh ("rope-length-int", [ropeTy], [rawIntTy], [])

    val ropeFromRangeOp =
	  newWithExh ("rope-from-range", 
		      [rawIntTy, rawIntTy, rawIntTy, rawIntTy],
		      [ropeTy], 
		      [])
    
  (* some hlops, not in the surface language, for use in rope maps *)
    val extractShortestRopeOp =
	let val retTy = BTy.T_Tuple (false, [ropeTy, listTy, rawIntTy])
	in
	    newWithExh ("extract-shortest-rope", [listTy], [retTy], [])
	end

    val curriedRopeSublistOp = let
	  val tTy = BTy.T_Tuple (false, [listTy, boolTy])
	  val fTy = BTy.T_Fun ([ropeTy], [exhTy], [tTy])
	  in
	    newWithExh ("curried-rope-sublist", [rawIntTy, rawIntTy], [fTy], [])
	  end

    val insertAtOp = newWithExh ("insert-at", [anyTy, listTy, rawIntTy], [listTy], [])

  (* extras *)
    val newImageOp = newWithExh ("image-new", [pairTy(intTy, intTy)], [BTy.T_Any], [])
    val updateImage3fOp = newWithExh (
	  "image-update3f",
	  [BTy.T_Tuple(false, [BTy.T_Any, intTy, intTy, floatTy, floatTy, floatTy])],
	  [unitTy], [])
    val updateImage3dOp = newWithExh (
	  "image-update3d",
	  [BTy.T_Tuple(false, [BTy.T_Any, intTy, intTy, doubleTy, doubleTy, doubleTy])],
	  [unitTy], [])
    val outputImageOp = newWithExh ("image-output", [pairTy(BTy.T_Any, stringTy)], [unitTy], [])
    val freeImageOp = newWithExh ("image-free", [BTy.T_Any], [unitTy], [])
    val getNumProcs = newWithExh ("get-num-procs", [unitTy], [intTy], [])
    val getNumVProcs = newWithExh ("get-num-vprocs", [unitTy], [intTy], [])

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
		future1SpawnOp,
		future1TouchOp,
		future1CancelOp,
                ropeSubOp,
		ropeLengthIntOp
	      ]	    

    type hlop_def = {
	name : BOM.hlop,			(* the HLOp's identifier *)
	inline : bool,				(* should the HLOp be inlined? *)
	def : BOM.lambda,			(* the HLOps definition *)
	pmlImports : (BOM.var * BOM.var) list,  (* imports from PML *)
	externs : (BOM.var * int) list		(* list of external variables (i.e., C functions) *)
						(* that def references paired with a count of the *)
						(* number of references *)
      }

    local 
    val hlops : hlop_def Stamp.Tbl.hash_table = Stamp.Tbl.mkTable(128, Fail "HLOp table")
    fun addDef (d as {name=name as HLOp.HLOp{id, name=n, ...}, inline, def, externs, pmlImports}) = 
	    Stamp.Tbl.insert hlops (id, d)
    in
    val addDefs = List.app addDef
    fun findDef (HLOp.HLOp{id, ...}) = Stamp.Tbl.find hlops id
    fun listHLOps () = List.map #def (Stamp.Tbl.listItems hlops)
    end

  end
