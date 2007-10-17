(* transfer-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate CFG control transfers.
 *)

signature TRANSFER = sig

    structure MTy : MLRISC_TYPES
    structure VarDef : VAR_DEF where MTy = MTy
    structure SpillLoc : SPILL_LOC

    val kfncRegs : CellsBasis.cell list
    val stdFuncRegs : CellsBasis.cell list
    val stdContRegs : CellsBasis.cell list

    type stms = MTy.T.stm list

  (* blocks *)
    val genGoto : VarDef.var_def_tbl -> CFG.jump -> stms

  (* known functions *)
    val genApply : VarDef.var_def_tbl -> {
	    f : CFG.var, args : CFG.var list
	  } -> {stms : stms, liveOut : MTy.T.mlrisc list}

  (* standard functions *)
    val genStdApply : VarDef.var_def_tbl -> {
	    f : CFG.var, clos : CFG.var, args : CFG.var list, ret : CFG.var, exh : CFG.var
	  } -> {stms : stms, liveOut : MTy.T.mlrisc list}

    val genStdThrow : VarDef.var_def_tbl -> {
	    k : CFG.var, clos : CFG.var, args : CFG.var list
	  } -> {stms : stms, liveOut : MTy.T.mlrisc list}


  (* perform a heap check, possibly triggering the GC *)
    val genHeapCheck : 
	VarDef.var_def_tbl -> {szb : word, nogc : CFG.jump}
	  -> {stms : stms, retKLbl : Label.label, retKStms : stms, liveOut : MTy.T.mlrisc list}

  (* promote an object to the global heap *)
    val genPromote : VarDef.var_def_tbl -> 
 	{frame : SpillLoc.frame, lhs: CFG.var, arg: CFG.var} -> 
		     {stms : stms, result : MTy.mlrisc_tree list}

  (* apply a C function f to args.  the result goes in lhs. *)
    val genCCall : VarDef.var_def_tbl ->
	{frame : SpillLoc.frame, lhs: CFG.var list, f : CFG.var, args: CFG.var list} -> 
		   {stms : stms, result : MTy.mlrisc_tree list}

  (* entry to a labelled function *)
    val genFuncEntry :
	VarDef.var_def_tbl ->
	(CFG.label * CFG.convention) -> stms

  end (* TRANSFER *)
