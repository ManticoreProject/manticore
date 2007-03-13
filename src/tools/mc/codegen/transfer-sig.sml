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

    val stdCallRegs : CellsBasis.cell list
    val stdContRegs : CellsBasis.cell list

    (* known functions *)
    val genGoto : VarDef.var_def_tbl -> CFG.jump -> MTy.T.stm list

    (* standard functions *)
    val genStdCall : VarDef.var_def_tbl -> 
	{f : CFG.var, clos : CFG.var, arg : CFG.var, ret : CFG.var, exh : CFG.var}
	-> {stms : MTy.T.stm list, liveOut : MTy.T.mlrisc list}

    val genStdThrow : VarDef.var_def_tbl -> 
	{k : CFG.var, clos : CFG.var, arg : CFG.var}
	-> {stms : MTy.T.stm list, liveOut : MTy.T.mlrisc list}

    (* perform a heap check, possibly triggering the GC *)
    val genHeapCheck : 
	VarDef.var_def_tbl ->
	{szb : word, nogc : CFG.jump}
	-> {stms : MTy.T.stm list, liveOut : MTy.T.mlrisc list}

    (* apply a C function f to args.  the result goes in lhs. *)
    val genCCall : VarDef.var_def_tbl ->
	{lhs: CFG.var list, f : CFG.var, args: CFG.var list} -> 
		   {stms : MTy.T.stm list, result : MTy.mlrisc_tree list}

    (* entry to a labelled function *)
    val genFuncEntry :
	VarDef.var_def_tbl ->
	(CFG.label * CFG.convention) -> MTy.T.stm list

end (* TRANSFER *)
