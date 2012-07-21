(* translate-range.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 * 
 * This module rewrites ranges in terms of calls to Rope.tabFromToP.
 *)

structure TranslateRange : sig

    val tr : AST.exp * AST.exp * AST.exp option * AST.ty -> AST.exp

  end = struct

    structure A = AST
    structure B = Basis
    structure T = Types
    structure DV = DelayedBasis.Var

  (* tr : A.exp * A.exp * A.exp option * A.ty -> A.exp *)
  (* This is simple because all the work is done in rope.pml. *)
    fun tr (fromExp, toExp, optStepExp, ty) = let
      val (rangeFnV, arg) =
       (case optStepExp
	  of NONE => (DV.ropeRangeNS (), A.TupleExp [fromExp, toExp])
	   | SOME stepExp => (DV.ropeRange (), A.TupleExp [fromExp, toExp, stepExp]))
      in 
        A.ApplyExp (A.VarExp (rangeFnV, []), arg, B.parrayTy B.intTy)
      end

  end
