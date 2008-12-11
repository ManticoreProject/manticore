(* rope-ops.sml
 *
 * COPYRIGHT (c) 2008 Manticore Group
 * All rights reserved.
 *
 * Translate operators like "parray_sub" to "Rope.sub".
 *)

structure RopeOps : sig

    val tr : AST.var -> AST.var

  end = struct

    fun ropeOp opname = 
     (case BasisEnv.getValFromBasis ["Ropes", opname]
        of ModuleEnv.Var oper => oper
	 | _ => raise Fail ("rope operator " ^ opname ^ " not found in basis")
        (* end case *))

    fun tr x = 
      if Var.same (x, Basis.parray_sub) then
        ropeOp "sub"
      else x

  end
