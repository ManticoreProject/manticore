(* atomic-ops-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Code generation for low-level atomic operations.
 *)

signature ATOMIC_OPS =
  sig

    structure MTy : MLRISC_TYPES

  (* atomic swap operation *)
    val genSwap : {
	    ty : MTy.T.ty,		(* either 32 or 64-bit *)
	    addr : MTy.T.rexp,		(* the location swapped *)
	    newVal : MTy.T.var		(* the new value to store *)
	  } -> (MTy.T.rexp * MTy.T.stm list)

  (* atomic compare and swap operation *)
    val genCompareAndSwap : {
	    ty : MTy.T.ty,		(* either 32 or 64-bit *)
	    addr : MTy.T.rexp,		(* the location begin tested *)
	    cmpVal : MTy.T.rexp,	(* the value to test against *)
	    newVal : MTy.T.rexp		(* the new value to store *)
	  } -> (MTy.T.ccexp * MTy.T.rexp * MTy.T.stm list)

  (* atomic test and set operation *)
    val genTestAndSet : {
	    ty : MTy.T.ty,		(* either 32 or 64-bit *)
	    addr : MTy.T.rexp		(* the location to be tested *)
	  } -> (MTy.T.ccexp * MTy.T.stm list)

  (* atomic fetch and add operation *)
    val genFetchAndAdd : {
	    ty : MTy.T.ty,		(* either 32 or 64-bit *)
	    addr : MTy.T.rexp,		(* the memory location *)
	    x : MTy.T.rexp	        (* the number to add *)
	  } -> (MTy.T.rexp * MTy.T.stm list)

  (* pause instruction to support efficient spin locks *)
    val genPause : unit -> MTy.T.stm list

  (* sequentializing operation for all write-to-memory instructions prior to this instruction *)
    val genFenceWrite : unit -> MTy.T.stm list

  (* sequentializing operation for all load-from-memory instructions prior to this instruction *)
    val genFenceRead : unit -> MTy.T.stm list

  (* sequentializing operation for all load-from-memory and write-to-memory instructions prior to this instruction *)
    val genFenceRW : unit -> MTy.T.stm list

  (* operation that returns the number of processor ticks counted by the TSC register *)
    val genTimeStampCounter : unit -> (MTy.T.rexp * MTy.T.stm list)

  end
