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
	    addr : MTy.T.rexp, 
            cmpVal : MTy.T.rexp, 
            newVal : MTy.T.rexp
	  } -> (MTy.T.ccexp * MTy.T.rexp * MTy.T.stm list)

  (* word-sized compare and swap operation *)
    val genCompareAndSwapWord : {
	    addr : MTy.T.rexp, 
            cmpVal : MTy.T.rexp, 
            newVal : MTy.T.rexp
	  } -> (MTy.T.ccexp * MTy.T.rexp * MTy.T.stm list)

  (* 32-bit test and set operation *)
    val genTestAndSet32 : {
	    addr : MTy.T.rexp,		(* the location swapped *)
	    newVal : MTy.T.var		(* the new value to store *)
	  } -> (MTy.T.rexp * MTy.T.stm list)

  (* 64-bit test and set operation *)
    val genTestAndSet64 : {
	    addr : MTy.T.rexp, 
            newVal : MTy.T.var
	  } -> (MTy.T.rexp * MTy.T.stm list)

  (* word-sized test and set operation *)
    val genTestAndSetWord : {
	    addr : MTy.T.rexp, 
            newVal : MTy.T.var
	  } -> (MTy.T.rexp * MTy.T.stm list)

  (* 32-bit fetch and add operation *)
    val genFetchAndAdd32 : {
	    addr : MTy.T.rexp,		(* the memory location *)
	    x : MTy.T.rexp	        (* the number to add *)
	  } -> (MTy.T.rexp * MTy.T.stm list)

  (* 64-bit fetch and add operation *)
    val genFetchAndAdd64 : {
	    addr : MTy.T.rexp,		(* the memory location *)
	    x : MTy.T.rexp	        (* the number to add *)
	  } -> (MTy.T.rexp * MTy.T.stm list)

  end
