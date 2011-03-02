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

  structure FB = FBasis
		 
  fun monoTy _ = raise Fail "todo: monoTy"
(*
    fun monoTy (F.TyScheme([], ty)) = U.prune ty
      | monoTy tys = U.toMonoTy tys
*)

(* exp : F.exp -> F.ty *)
  fun exp (F.LetExp (_, e)) = exp e
    | exp (F.IfExp (_, _, _, t)) = t
    | exp (F.FunExp (param, _, r)) =
        T.FunTy (V.monoTypeOf param, r)
    | exp (F.CaseExp (_, _, t)) = t
    | exp (F.PCaseExp (_, _, t)) = t
    | exp (F.HandleExp (_, _, t)) = t
    | exp (F.RaiseExp (_, t)) = t
    | exp (F.ApplyExp (_, _, t)) = t
    | exp (F.VarArityOpExp (_, _, t)) = raise Fail "unsupported"
    | exp (F.TupleExp (es, intfTy)) = T.TupleTy (intfTy, List.map exp es)
    | exp (F.RangeExp (_, _, _, t)) = FB.parrayTy t 
    | exp (F.PTupleExp es) = let
        val ts = List.map exp es
        val is = List.map U.interfaceTy ts
        in
          T.TupleTy (AST.TupleTy is, ts)
        end
    | exp (F.PArrayExp (_, t)) = FB.parrayTy t
    | exp (f as F.FArrayExp (_, n, t)) = let
        val nt = ntree n
        in
          T.FlatArrayTy (FB.parrayTy t, nt)
	end	  
    | exp (F.PCompExp (e, _, _)) = FB.parrayTy (exp e)
    | exp (F.PChoiceExp (_, t)) = t
    | exp (F.SpawnExp _) = FB.threadIdTy
    | exp (F.ConstExp c) = const c
    | exp (F.VarExp (x, argTys)) = 
        (U.apply (V.typeOf x, argTys) 
	 handle ex => (print(concat["typeOf(", V.toString x, ")\n"]); raise ex))
    | exp (F.SeqExp (_, e)) = exp e
    | exp (F.OverloadExp (ref (F.Instance x))) =
      (* NOTE: all overload instances are monomorphic *)
        monoTy (FTVar.typeOf x)
    | exp (F.OverloadExp _) = raise Fail "unresolved overloading"
    | exp (F.ExpansionOptsExp (opts, e)) = exp e
					   
  and ntree (F.Lf _) = N.Lf
    | ntree (F.Nd []) = raise Fail "ntree"
    | ntree (F.Nd (n::ns)) = let
      (* find the max-depth subtree *)
        fun lp ([], acc) = acc
	  | lp (h::t, acc) = let
              val curr = ntree h
              in
		if N.deeper (curr, acc) then lp (t, curr)
		else lp (t, acc)
	      end
        in
	  lp (ns, ntree n)
        end

  and const (F.DConst (dc, argTys)) = FTDataCon.typeOf' (dc, argTys)
    | const (F.LConst (_, t)) = t
				
  and pat (F.ConPat (dc, argTys, _)) = FTDataCon.resultTypeOf' (dc, argTys)
    | pat (F.TuplePat ps) = let
        val ts = List.map pat ps
	val is = List.map U.interfaceTy ts
        in
          T.TupleTy (AST.TupleTy is, ts)
        end
    | pat (F.VarPat x) = monoTy (V.typeOf x)
    | pat (F.WildPat t) = t
    | pat (F.ConstPat c) = const c
			   
  and ppat (F.NDWildPat t) = t
    | ppat (F.HandlePat (_, t)) = t
    | ppat (F.Pat p) = pat p
		       
end
