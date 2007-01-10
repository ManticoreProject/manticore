(* label-code-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature LABEL_CODE =
  sig

    structure T : MLTREE
      where Region = ManticoreRegion
    structure MTy : MLRISC_TYPES
      where T = T

    val getName : CFG.label -> T.label

  (* get the psuedo registers that are the parameters for the block
   * attached to this label.
   *)
    val paramRegs : CFG.label -> MTy.mlrisc_reg

  end
