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

    fun same (Var x, Var y) = BV.same(x, y)
      | same (Int a, Int b) = (a = b)
      | same (Enum a, Enum b) = (a = b)
      | same _ = false

    fun bind x = (case BV.kindOf x
	  of B.VK_RHS(B.E_Const(Literal.Int n, _)) => Int n
	   | B.VK_RHS(B.E_Const(Literal.Enum n, _)) => Enum n
	   | B.VK_RHS(B.E_Prim p) => Prim p
	   | _ => Var x
	 (* end case *))

    fun contract (env, x, p) = let
	  fun eq (a, b) = if same(a, b)
		then (
		  PrimUtil.app Census.decUseCnt p;
		  OK([([x], B.E_Const(Literal.trueLit, BTy.boolTy))], env))
		else FAIL
	  fun neq (a, b) = if same(a, b)
		then (
		  PrimUtil.app Census.decUseCnt p;
		  OK([([x], B.E_Const(Literal.falseLit, BTy.boolTy))], env))
		else FAIL
	  in
	    case PrimUtil.map bind p
	     of P.isBoxed(Enum _) => (
		  PrimUtil.app Census.decUseCnt p;
		  OK([([x], B.E_Const(Literal.falseLit, BTy.boolTy))], env))
	      | P.isUnboxed(Enum _) => (
		  PrimUtil.app Census.decUseCnt p;
		  OK([([x], B.E_Const(Literal.trueLit, BTy.boolTy))], env))
	      | P.Equal(a, b) => eq(a, b)
	      | P.NotEqual(a, b) => neq(a, b)
	      | P.BEq(a, b) => eq(a, b)
	      | P.BNEq(a, b) => neq(a, b)
	      | P.I32Eq(a, b) => eq(a, b)
	      | P.I32NEq(a, b) => neq(a, b)
	      | P.I64Eq(a, b) => eq(a, b)
	      | P.I64NEq(a, b) => neq(a, b)
	      | P.F32Eq(a, b) => eq(a, b)
	      | P.F32NEq(a, b) => neq(a, b)
	      | P.F64Eq(a, b) => eq(a, b)
	      | P.F64NEq(a, b) => neq(a, b)
	      | _ => FAIL
	    (* end case *)
	  end

  end
