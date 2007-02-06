signature TRANSFER = sig

    structure MTy : MLRISC_TYPES
    structure VarDef : VAR_DEF where MTy = MTy
    structure SpillLoc : SPILL_LOC

    val stdCallRegs : CellsBasis.cell list
    val stdContRegs : CellsBasis.cell list

    val genGCCall : unit -> MTy.T.stm list

    val genGoto : VarDef.var_def_tbl -> CFG.jump -> MTy.T.stm list

    val genStdTransfer : VarDef.var_def_tbl -> 
	(CFG.var * MTy.T.controlflow * CFG.var list * MTy.T.var list)
	-> {stms : MTy.T.stm list, liveOut : MTy.T.mlrisc list}

    val genHeapCheck : 
	VarDef.var_def_tbl ->
	{szb : word, gc : CFG.jump, nogc : CFG.jump}
	-> MTy.T.stm list

    val genLabelEntry :
	VarDef.var_def_tbl ->
	(CFG.label * CFG.convention) -> MTy.T.stm list

end (* TRANSFER *)
