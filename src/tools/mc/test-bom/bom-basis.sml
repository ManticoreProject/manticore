(* bom-basis.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Predefined high-level operations.
 *)

structure Basis =
  struct

    local
      structure BTy = BOMTy
      structure H = HLOp
      fun new (name, params, res, attrs) =
	    H.new(Atom.atom name, {params=params, results=res}, attrs)
    (* some standard parameter types *)
      val vprocTy = BTy.T_Any	(* FIXME *)
      val fiberTy = BTy.T_Cont[]
      val sigTy = BTy.T_Any	(* FIXME: really either Enum(0) or fiberTy *)
      val sigActTy = BTy.T_Cont[sigTy]
      val
    in

(*
    val qItemAlloc of var list  (* allocate a queue item *)
    val qEnqueue of (var * var) (* insert an item [nonatomic] *)
    val qDequeue of var 	(* remove an item [nonatomic] *)
    val qEmpty of var		(* return true if queue is empty [nonatomic] *)

  (* concurrent queue operations *)
    val atomicQEnqueue of (var * var)	(* insert an item [atomic] *)
    val atomicQDequeue of var		(* remove an item [atomic] *)
*)

  (* scheduler operations *)
    val runOp = new("run", [vprocTy, sigActTy, fiberTy], [], [H.NORETURN])
    val forwardOp = new("forward", [vprocTy, sigTy], [], [H.NORETURN])
    val dequeueOp = new("dequeue", [vprocTy], [queueItemTy], [])
    val enqueue = new("enqueue", [vprocTy, tidTy, fiberTy], [], [])

    end (* local *)
  end
