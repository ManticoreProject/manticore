(* hlop-env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
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

  (* scheduler operations *)
    val runOp : HLOp.hlop
    val forwardOp : HLOp.hlop
    val dequeueOp : HLOp.hlop
    val enqueueOp : HLOp.hlop

    val define : HLOp.hlop -> unit
    val find : Atom.atom -> HLOp.hlop option

  end = struct

    structure H = HLOp
    structure BTy = BOMTy
    structure Basis = BOMBasis

  (* some standard parameter types *)
    val vprocTy = BTy.T_Any	(* FIXME *)
    val fiberTy = BTy.T_Cont[]
    val sigTy = BTy.T_Any	(* FIXME: really either Enum(0) or fiberTy *)
    val sigActTy = BTy.T_Cont[sigTy]
    val tidTy = BTy.tidTy
    val exhTy = BTy.exhTy
    val listTy = BTy.T_TyCon Basis.listTyc

    fun new (name, params, res, attrs) =
	  H.new(Atom.atom name, {params= List.map HLOp.PARAM params, exh=[], results=res}, attrs)

  (* high-level operations used to implement Manticore language constructs *)
    val listAppendOp = new("list-append", [listTy, listTy], [], [])
    val spawnOp = new("spawn", [BTy.T_Fun([], [exhTy], [])], [tidTy], [])
    val threadExitOp = new("thread-exit", [], [], [H.NORETURN])

  (* scheduler operations *)
    val runOp = new("run", [vprocTy, sigActTy, fiberTy], [], [H.NORETURN])
    val forwardOp = new("forward", [vprocTy, sigTy], [], [H.NORETURN])
    val dequeueOp = new("dequeue", [vprocTy], [Basis.rdyqItemTy], [])
    val enqueueOp = new("enqueue", [vprocTy, tidTy, fiberTy], [], [])

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
		dequeueOp,
		enqueueOp,
		forwardOp,
		runOp
	      ]

  end
