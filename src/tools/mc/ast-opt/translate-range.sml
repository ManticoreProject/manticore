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

  (* tr : A.exp * A.exp * A.exp option * A.ty -> A.exp *)
  (* This is simple because all the work is done in ropes.pml. *)
    fun tr (fromExp, toExp, optStepExp, ty) = let
      fun get v = BasisEnv.getVarFromBasis ["Ropes", v]
      val (rangeFnV, arg) =
       (case optStepExp
	  of NONE => (get "rangePNoStep", A.TupleExp [fromExp, toExp])
	   | SOME stepExp => (get "rangeP", A.TupleExp [fromExp, toExp, stepExp]))
      in 
        A.ApplyExp (A.VarExp (rangeFnV, []), arg, PArray.parrayTy Basis.intTy)
      end

  end
