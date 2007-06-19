(* prim-contract.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimContract : sig

  (* result of primop contraction *)
    datatype const_fold_result
      = FAIL
      | OK of ((BOM.var list  * BOM.rhs) list * BOMUtil.subst)

    val contract : (BOMUtil.subst * BOM.var * BOM.prim) -> const_fold_result

  end = struct

  (* result of primop contraction *)
    datatype const_fold_result
      = FAIL
      | OK of ((BOM.var list * BOM.rhs) list * BOMUtil.subst)

    fun contract (env, x, p) = FAIL

  end
