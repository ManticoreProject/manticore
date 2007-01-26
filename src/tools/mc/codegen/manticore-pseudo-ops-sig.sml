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

    val text : pseudo_op
    val global : Label.label -> pseudo_op

    structure PseudoOps : PSEUDO_OPS 
	  where T = P.T
	  and type Client.pseudo_op = pseudo_op_ext

end (* MANTICORE_PSEUDO_OPS *)
