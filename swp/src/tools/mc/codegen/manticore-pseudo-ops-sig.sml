(* manticore-pseudo-ops-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * MLRISC pseudo ops needed for code generation.
 *)

signature MANTICORE_PSEUDO_OPS = sig

    structure P : PSEUDO_OPS_BASIS
    type pseudo_op_ext
    type pseudo_op = (P.T.labexp, pseudo_op_ext) PseudoOpsBasisTyp.pseudo_op

    datatype int_size = I8 | I16 | I32 | I64 | Iptr

    val text : pseudo_op
    val global : Label.label -> pseudo_op
    val rodata : pseudo_op
    val alignData : pseudo_op
    val alignCode : pseudo_op
    val alignEntry : pseudo_op
    val float : (P.T.fty * FloatLit.float list) -> pseudo_op
    val asciz : string -> pseudo_op
    val int : (int_size * IntInf.int list) -> pseudo_op

    structure PseudoOps : PSEUDO_OPS 
	  where T = P.T
	  and type Client.pseudo_op = pseudo_op_ext

end (* MANTICORE_PSEUDO_OPS *)
