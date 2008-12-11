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

    fun tr x = let
      fun xeq y = Var.same (x, y)
      in
        if      xeq Basis.parray_sub then ropeOp "sub"
	else if xeq Basis.parray_len then ropeOp "length"
	else x
      end

  end
