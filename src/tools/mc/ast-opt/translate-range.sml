(* translate-range.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites ranges in terms of calls to PArray.range.
 *)

structure TranslateRange : sig

    val tr : AST.exp * AST.exp * AST.exp option * AST.ty -> AST.exp

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure DV = DelayedBasis.Var
    structure AU = ASTUtil
    structure TU = TypeUtil

  (* tr : A.exp * A.exp * A.exp option * A.ty -> A.exp *)
  (* TODO right now this only works at type int; it's designed otherwise *)
    fun tr (fromExp, toExp, optStepExp, ty) = let
      val _ = if TU.same (ty, B.intTy) then () else raise Fail "not int"
      val mkRng = A.VarExp (DV.parrayRange (), [])
      val stepExp = (case optStepExp
        of SOME e => e
	 | NONE => AU.mkInt 1
        (* end case *))
      in
        AU.mkApplyExp (mkRng, [fromExp, toExp, stepExp])
      end

  end
