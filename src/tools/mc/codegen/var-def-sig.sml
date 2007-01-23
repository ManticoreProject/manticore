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
    val setDefOf : var_def_tbl -> (CFG.var * MTy.mlrisc_tree) -> unit

    val defOf : var_def_tbl -> CFG.var -> MTy.T.rexp
    val fdefOf : var_def_tbl -> CFG.var -> MTy.T.fexp
    val cdefOf : var_def_tbl -> CFG.var -> MTy.T.ccexp

end (* VAR_DEF *)
