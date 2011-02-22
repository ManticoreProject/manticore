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
  structure T = FTTypes
  structure N = NestingTreeTypes
  structure U = FTTypeUtil
  structure V = FTVar
		 
  fun monoTy _ = raise Fail "todo: monoTy"
(*
    fun monoTy (F.TyScheme([], ty)) = U.prune ty
      | monoTy tys = U.toMonoTy tys
*)

(* exp : F.exp -> F.ty *)
  fun exp (F.LetExp (_, e)) = exp e
    | exp (F.IfExp (_, _, _, t)) = t
    | exp (F.FunExp (param, _, t)) = T.FunTy (V.monoTypeOf param, t)
    | exp (F.CaseExp (_, _, t)) = t
    | exp (F.PCaseExp (_, _, t)) = t
    | exp (F.HandleExp (_, _, t)) = t
    | exp (F.RaiseExp (_, t)) = t
    | exp (F.ApplyExp (_, _, t)) = t
    | exp (F.VarArityOpExp (_, _, t)) = t
    | exp (F.TupleExp (es, intfTy)) = let
        val rs = List.map (T.reprTy o exp) es
        in
	  T.IR (intfTy, R.TupleTy rs)
        end
    | exp (F.RangeExp (_, _, _, t)) = B.parrayTy t
    | exp (F.PTupleExp es) = T.TupleTy(List.map exp es)
    | exp (F.PArrayExp (_, t)) = B.parrayTy t
    | exp (F.FArrayExp (_, n, t)) = let
        val nt = ntree t
(*	val ft = R.FlatArrayTy ( FIXME *)
        in
	  raise Fail "todo"
        end
    | exp (F.PCompExp (e, _, _)) = B.parrayTy(exp e)
    | exp (F.PChoiceExp (_, t)) = t
    | exp (F.SpawnExp _) = B.threadIdTy
    | exp (F.ConstExp c) = const c
    | exp (F.VarExp (x, argTys)) = 
        (U.apply (Var.typeOf x, argTys) 
	 handle ex => (print(concat["typeOf(", V.toString x, ")\n"]); raise ex))
    | exp (F.SeqExp (_, e)) = exp e
    | exp (F.OverloadExp (ref (F.Instance x))) =
      (* NOTE: all overload instances are monomorphic *)
        monoTy (Var.typeOf x)
    | exp (F.OverloadExp _) = raise Fail "unresolved overloading"
    | exp (F.ExpansionOptsExp (opts, e)) = exp e
					   
  and ntree (F.Lf _) = N.Lf
    | ntree (F.Nd n) = N.Nd (ntree n)

  and const (F.DConst (dc, argTys)) = DataCon.typeOf' (dc, argTys)
    | const (F.LConst (_, t)) = t
				
  and pat (F.ConPat (dc, argTys, _)) = DataCon.resultTypeOf' (dc, argTys)
    | pat (F.TuplePat ps) = T.TupleTy (List.map pat ps)
    | pat (F.VarPat x) = monoTy (V.typeOf x)
    | pat (F.WildPat t) = t
    | pat (F.ConstPat c) = const c
			   
  and ppat (F.NDWildPat t) = t
    | ppat (F.HandlePat (_, t)) = t
    | ppat (F.Pat p) = pat p
		       
end
