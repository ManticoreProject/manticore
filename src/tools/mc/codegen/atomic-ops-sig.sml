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

  (* 32-bit compare and swap operation *)
    val genCompareAndSwap32 : {
	    addr : MTy.T.rexp,		(* the location begin tested *)
	    cmpVal : MTy.T.rexp,	(* the value to test against *)
	    newVal : MTy.T.rexp		(* the new value to store *)
	  } -> (MTy.T.ccexp * MTy.T.rexp * MTy.T.stm list)

  (* 64-bit compare and swap operation *)
    val genCompareAndSwap64 : {
	    addr : MTy.T.rexp, cmpVal : MTy.T.rexp, newVal : MTy.T.rexp
	  } -> (MTy.T.ccexp * MTy.T.rexp * MTy.T.stm list)

  (* word-sized compare and swap operation *)
    val genCompareAndSwapWord : {
	    addr : MTy.T.rexp, cmpVal : MTy.T.rexp, newVal : MTy.T.rexp
	  } -> (MTy.T.ccexp * MTy.T.rexp * MTy.T.stm list)

  end
