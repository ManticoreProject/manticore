(* var-def-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A lookup table for Manticore variables.
 *)

signature VAR_DEF = sig

    structure MTy : MLRISC_TYPES

    type var_def_tbl

    val newTbl : unit -> var_def_tbl
    val getDefOf : var_def_tbl -> CFG.var -> MTy.mlrisc_tree
    val useDefOf : var_def_tbl -> CFG.var -> MTy.mlrisc_tree
    val setDefOf : var_def_tbl -> (CFG.var * MTy.mlrisc_tree) -> unit

    val defOf : var_def_tbl -> CFG.var -> MTy.T.rexp
    val fdefOf : var_def_tbl -> CFG.var -> MTy.T.fexp
    val cdefOf : var_def_tbl -> CFG.var -> MTy.T.ccexp

    val clear : var_def_tbl -> unit

    (* binds an expression in the table, but possibly returns 
     * code that binds the expression to a register.  *) 
    val gprBind : var_def_tbl -> (MTy.T.ty * CFG.var * MTy.T.rexp) -> MTy.T.stm list
    val cbind : var_def_tbl -> (CFG.var * MTy.T.ccexp) -> MTy.T.stm list
    val fbind : var_def_tbl -> (MTy.T.fty * CFG.var * MTy.T.fexp) -> MTy.T.stm list

    val bind : var_def_tbl -> (CFG.var * MTy.mlrisc_tree) -> MTy.T.stm list

    (* force all pending loads into registers *)
    val flushLoads : var_def_tbl -> MTy.T.stm list

end (* VAR_DEF *)
