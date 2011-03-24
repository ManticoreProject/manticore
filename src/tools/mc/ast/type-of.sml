(* type-of.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute the type of AST expressions, patterns, etc.
 *)

structure TypeOf : sig

    val exp   : AST.exp   -> Types.ty
    val pat   : AST.pat   -> Types.ty
    val const : AST.const -> Types.ty
    val ppat  : AST.ppat  -> Types.ty

  end = struct

    structure Ty = Types
    structure B = Basis
    structure TU = TypeUtil

    fun monoTy (Ty.TyScheme([], ty)) = TU.prune ty
      | monoTy tys = TU.toMonoTy tys

    fun exp (AST.LetExp(_, e)) = exp e
      | exp (AST.IfExp(_, _, _, ty)) = ty
      | exp (AST.FunExp(param, _, ty)) = Ty.FunTy(Var.monoTypeOf param, ty)
      | exp (AST.CaseExp(_, _, ty)) = ty
      | exp (AST.PCaseExp(_, _, ty)) = ty
      | exp (AST.HandleExp (_, _, ty)) = ty
      | exp (AST.RaiseExp (_, ty)) = ty
      | exp (AST.ApplyExp(_, _, ty)) = ty
      | exp (AST.VarArityOpExp (_, _, ty)) = ty
      | exp (AST.TupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.RangeExp(_, _, _, ty)) = B.parrayTy ty
      | exp (AST.PTupleExp es) = Ty.TupleTy(List.map exp es)
      | exp (AST.PArrayExp(_, ty)) = B.parrayTy ty
      | exp (AST.PCompExp(e, _, _)) = B.parrayTy(exp e)
      | exp (AST.PChoiceExp(_, ty)) = ty
      | exp (AST.SpawnExp _) = B.threadIdTy
      | exp (AST.ConstExp c) = const c
      | exp (AST.VarExp(x, argTys)) = 
          (TU.apply (Var.typeOf x, argTys) 
	   handle ex => (print(concat["typeOf(", Var.toString x, ")\n"]); 
			 print "\n";
			 raise ex))
      | exp (AST.SeqExp(_, e)) = exp e
      | exp (AST.OverloadExp(ref(AST.Instance x))) =
	(* NOTE: all overload instances are monomorphic *)
	  monoTy (Var.typeOf x)
      | exp (AST.OverloadExp _) = raise Fail "unresolved overloading"
      | exp (AST.ExpansionOptsExp (opts, e)) = exp e
      | exp (AST.FTupleExp es) = Ty.TupleTy (List.map exp es)
      | exp (AST.FArrayExp (_, n, ty)) = Ty.FArrayTy (ty, ntree n) 
      | exp (AST.FlOp oper) = FlattenOp.typeOf oper (* fl_op oper *)

    and ntree (AST.Lf _) = Ty.LfTy
      | ntree (AST.Nd ns) = (Ty.NdTy o TU.maxNTree o List.map ntree) ns

    and const (AST.DConst(dc, argTys)) = DataCon.typeOf'(dc, argTys)
      | const (AST.LConst(_, ty)) = ty

    and pat (AST.ConPat(dc, argTys, _)) = DataCon.resultTypeOf' (dc, argTys)
      | pat (AST.TuplePat ps) = Ty.TupleTy(List.map pat ps)
      | pat (AST.VarPat x) = monoTy (Var.typeOf x)
      | pat (AST.WildPat ty) = ty
      | pat (AST.ConstPat c) = const c

    and ppat (AST.NDWildPat ty) = ty
      | ppat (AST.HandlePat (_, ty)) = ty
      | ppat (AST.Pat p) = pat p

  end
