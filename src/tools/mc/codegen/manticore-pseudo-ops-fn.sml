(* manticore-pseudo-ops-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * MLRISC pseudo ops needed for code generation.
 *)

functor ManticorePseudoOpsFn (
	structure P : PSEUDO_OPS_BASIS) : MANTICORE_PSEUDO_OPS = struct

  structure P = P
  structure PTy = PseudoOpsBasisTyp

  type pseudo_op_ext = unit

  type pseudo_op = unit P.pseudo_op

  val text : pseudo_op = PTy.TEXT
  fun global lab = PTy.EXPORT [lab]

  structure Client = struct
      structure AsmPseudoOps = P
      type pseudo_op = pseudo_op_ext
		       
      fun toString () = ""

      fun emitValue _ = raise Fail "todo"
      fun sizeOf _ = raise Fail "todo"
      fun adjustLabels _ = raise Fail "todo"
  end (* Client *)

  structure PseudoOps = PseudoOps (structure Client = Client)

end (* ManticorePseudoOpsFn *)
