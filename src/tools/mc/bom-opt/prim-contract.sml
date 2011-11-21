(* prim-contract.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimContract : sig

  (* result of primop contraction *)
    datatype const_fold_result
      = FAIL
      | OK of ((BOM.var list  * BOM.rhs) list * BOMUtil.subst)

    val contract : (BOMUtil.subst * BOM.var * BOM.prim) -> const_fold_result

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure P = Prim

  (* result of primop contraction *)
    datatype const_fold_result
      = FAIL
      | OK of ((B.var list * B.rhs) list * BOMUtil.subst)

    datatype binding
      = Var of B.var
      | Prim of B.prim
      | Int of IntInf.int
      | Enum of word

    fun bind x = (case BV.kindOf x
	  of B.VK_RHS(B.E_Const(Literal.Int n, _)) => Int n
	   | B.VK_RHS(B.E_Const(Literal.Enum n, _)) => Enum n
	   | B.VK_RHS(B.E_Prim p) => Prim p
	   | _ => Var x
	 (* end case *))

    fun contract (env, x, p) = FAIL

  end
