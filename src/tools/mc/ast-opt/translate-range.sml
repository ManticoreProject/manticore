(* translate-range.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites ranges in terms of calls to Ropes.tabFromToP.
 *)

structure TranslateRange : sig

    val tr : AST.exp * AST.exp * AST.exp option * AST.ty -> AST.exp

end = struct

    structure A = AST
    structure T = Types

    val changed = ref false

    val ONE = ASTUtil.mkInt 1

    fun mkPlus (e1, e2, ty) = A.ApplyExp (A.VarExp (Basis.int_plus, []),
					  A.TupleExp [e1, e2],
					  ty)

    fun incr e = mkPlus (e, ONE, Basis.intTy)

  (* tr : A.exp * A.exp * A.exp option * A.ty -> A.exp *)
    fun tr (loExp, hiExp, NONE, ty) = let
          val loT = TypeOf.exp loExp
	  val hiT = TypeOf.exp hiExp
	  val loV = Var.new ("lo", loT) (* these types should all be int *)
	  val hiV = Var.new ("hi", hiT)
	  val nV = Var.new ("n", loT)
	  val loE = A.VarExp (loV, [])
	  val hiE = A.VarExp (hiV, [])
	  val nE = A.VarExp (nV, [])
	  val body = mkPlus (nE, loE, loT)
	  val f = A.FunExp (nV, body, loT)
	  val loBind = A.ValBind (A.VarPat loV, loExp)
	  val hiBind = A.ValBind (A.VarPat hiV, hiExp)
	  val tabFromToP = BasisEnv.getVarFromBasis ["Ropes", "tabFromToP"]
	  (* increment hiE b/c tabFromToP is exclusive of its upper bound *)
	  val body = A.ApplyExp (A.VarExp (tabFromToP, [loT]),
				 A.TupleExp [loE, incr hiE, f],
				 PArray.parrayTy loT)
          in
            ASTUtil.mkLetExp ([loBind, hiBind], body)
          end
      | tr (loExp, hiExp, SOME stepExp, ty) = raise Fail "steps in ranges: todo"

  end
