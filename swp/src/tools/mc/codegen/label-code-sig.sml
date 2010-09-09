(* label-code-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Lookup information attached to labels.
 *)

signature LABEL_CODE = sig

    structure MTy : MLRISC_TYPES

    val getName : CFG.label -> Label.label

    (* Get and set the parameters associated with the block assigned to this
     * label. *)
    val setParamRegs : (CFG.label * MTy.mlrisc_reg list) -> unit
    val getParamRegs : CFG.label -> MTy.mlrisc_reg list

end (* LABEL_CODE *)
