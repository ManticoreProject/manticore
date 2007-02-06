(* code-gen-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate the CFG representation into MLRISC trees.
 *)

signature CODE_GEN = sig

    val codeGen : {
	dst : TextIO.outstream,  (* assembly output stream *)
	code : CFG.module        (* first-order CPS program *)
    } -> unit

end (* CODE_GEN *)
