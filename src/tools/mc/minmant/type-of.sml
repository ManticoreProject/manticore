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
    structure TU = TypeUtil

    fun monoTy (Ty.TyScheme([], ty)) = TU.prune ty
      | monoTy _ = raise Fail "unexpected type scheme"

    fun exp (AST.LetExp(_, e)) = exp e
      | exp (AST.IfExp(_, _, _, ty)) = ty
      | exp (AST.CaseExp(_, _, ty)) = ty
      | exp (AST.ApplyExp(_, _, ty)) = ty
      | exp (AST.TupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.RangeExp(_, _, _, ty)) = B.parrayTy ty
      | exp (AST.PTupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.PArrayExp(_, ty)) = B.parrayTy ty
      | exp (AST.ComprehendExp(e, _, _)) = B.parrayTy(exp e)
      | exp (AST.SpawnExp _) = Basis.threadIdTy
      | exp (AST.ConstExp c) = const c
      | exp (AST.VarExp(x, argTys)) = TU.apply(Var.typeOf x, argTys)
      | exp (AST.SeqExp(_, e)) = exp e
      | exp (AST.OverloadExp(ref(Instance x))) =
	(* NOTE: all overload instances are monomorphic *)
	  monoTy (Var.typeOf x)

    and const (AST.DConst(dc, argTys)) = TU.apply(DataCon.typeOf dc, argTys)
      | const (AST.LConst(_, ty)) = ty

    and pat (AST.ConPat(dc, argTys, _)) = let
	  val Ty.FunTy(_, ty) = TU.apply(DataCon.typeOf dc, argTys)
	  in
	    ty
	  end
      | pat (AST.TuplePat ps) = Ty.TupleTy(List.map pat es)
      | pat (AST.VarPat x) = monoTy (Var.typeOf x)
      | pat (AST.ConstPat c) = const c

  end
