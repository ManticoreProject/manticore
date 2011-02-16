(* ft-type-of.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute the type of FLAST expressions, patterns, etc.
 *)

structure FTTypeOf : sig

    val exp   : FLAST.exp   -> FTTypes.ty
    val pat   : FLAST.pat   -> FTTypes.ty
    val const : FLAST.const -> FTTypes.ty
    val ppat  : FLAST.ppat  -> FTTypes.ty

  end = struct

    structure F = FLAST
    structure Ty = FTTypes
    structure TU = FTTypeUtil
    structure V  = FTVar

    fun monoTy _ = raise Fail "todo: monoTy"
(*
    fun monoTy (FLF.TyScheme([], ty)) = TU.prune ty
      | monoTy tys = TU.toMonoTy tys
*)

    fun exp (F.LetExp(_, e)) = exp e
      | exp (F.IfExp(_, _, _, ty)) = ty
      | exp (F.FunExp(param, _, ty)) = Ty.FunTy (V.monoTypeOf param, ty)
      | exp (F.CaseExp(_, _, ty)) = ty
      | exp (F.PCaseExp(_, _, ty)) = ty
      | exp (F.HandleExp (_, _, ty)) = ty
      | exp (F.RaiseExp (_, ty)) = ty
      | exp (F.ApplyExp(_, _, ty)) = ty
      | exp (F.VarArityOpExp (_, _, ty)) = ty
      | exp (F.TupleExp es) = Ty.TupleTy (List.map exp es)
      | exp (F.RangeExp(_, _, _, ty)) = B.parrayTy ty
      | exp (F.PTupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (F.PArrayExp(_, ty)) = B.parrayTy ty
      | exp (F.PCompExp(e, _, _)) = B.parrayTy(exp e)
      | exp (F.PChoiceExp(_, ty)) = ty
      | exp (F.SpawnExp _) = B.threadIdTy
      | exp (F.ConstExp c) = const c
      | exp (F.VarExp(x, argTys)) = 
          (TU.apply(Var.typeOf x, argTys) 
	   handle ex => (print(concat["typeOf(", V.toString x, ")\n"]); raise ex))
      | exp (F.SeqExp(_, e)) = exp e
      | exp (F.OverloadExp(ref(F.Instance x))) =
	(* NOTE: all overload instances are monomorphic *)
	  monoTy (Var.typeOf x)
      | exp (F.OverloadExp _) = raise Fail "unresolved overloading"
      | exp (F.ExpansionOptsExp (opts, e)) = exp e

    and const (F.DConst(dc, argTys)) = DataCon.typeOf'(dc, argTys)
      | const (F.LConst(_, ty)) = ty

    and pat (F.ConPat(dc, argTys, _)) = DataCon.resultTypeOf' (dc, argTys)
      | pat (F.TuplePat ps) = Ty.TupleTy(List.map pat ps)
      | pat (F.VarPat x) = monoTy (V.typeOf x)
      | pat (F.WildPat ty) = ty
      | pat (F.ConstPat c) = const c

    and ppat (F.NDWildPat ty) = ty
      | ppat (F.HandlePat (_, ty)) = ty
      | ppat (F.Pat p) = pat p

  end
