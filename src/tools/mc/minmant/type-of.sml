(* type-of.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute the type of AST expressions, patterns, etc.
 *)

structure TypeOf : sig

    val exp : AST.exp -> Types.ty
    val pat : AST.pat -> Types.ty
    val const : AST.const -> Types.ty

  end = struct

    structure Ty = Types
    structure B = Basis

    fun exp (AST.LetExp(_, e)) = exp e
      | exp (AST.IfExp(_, _, _, ty)) = ty
      | exp (AST.CaseExp(_, _, ty)) = ty
      | exp (AST.ApplyExp(_, _, ty)) = ty
      | exp (AST.TupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.RangeExp(_, _, _, ty)) = B.parrayTy ty
      | exp (AST.PTupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.PArrayExp of exp list
      | exp (AST.ComprehendExp(e, _, _)) = B.parrayTy(exp e)
      | exp (AST.SpawnExp _) = Basis.threadIdTy
      | exp (AST.ConstExp of const
      | exp (AST.VarExp(x, argTys)) = (* apply typeOf dc to argTys *)
      | exp (AST.SeqExp of (exp * exp)
      | exp (AST.OverloadExp of overload_var ref

    and const (AST.DConst(dc, argTys)) = (* apply typeOf dc to argTys *)
      | const (AST.LConst(_, ty)) = ty
