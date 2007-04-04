(* unify.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Unify : sig

  (* destructively unify two types; return true if successful, false otherwise *)
    val unify : AST.ty * AST.ty -> bool

  end = struct

    structure MV = MetaVar
    structure TU = TypeUtil

  (* does a meta-variable occur in a type? *)
    fun occursIn (mv, ty) = let
	  fun occurs ty = (case TU.prune ty
		 of AST.ErrorTy => false
		  | (AST.MetaTy mv') => MV.same(mv, mv')
		  | (AST.VarTy _) => raise Fail "unexpected type variable"
		  | (AST.ConTy(args, _)) => List.exists occurs args
		  | (AST.FunTy(ty1, ty2)) => occurs ty1 orelse occurs ty2
		  | (AST.TupleTy tys) => List.exists occurs tys
		(* end case *))
	  in
	    occurs ty
	  end

  (* adjust the depth of any non-instantiated meta-variable that is bound
   * deeper than the given depth.
   *)
    fun adjustDepth (ty, depth) = let
	  fun adjust AST.ErrorTy = ()
	    | adjust (AST.MetaTy(AST.MVar{info as ref(AST.UNIV d), ...})) =
		if (depth < d) then info := AST.UNIV d else ()
	    | adjust (AST.MetaTy(AST.MVar{info=ref(AST.INSTANCE ty), ...})) = adjust ty
	    | adjust (AST.VarTy _) = raise Fail "unexpected type variable"
	    | adjust (AST.ConTy(args, _)) = List.app adjust args
	    | adjust (AST.FunTy(ty1, ty2)) = (adjust ty1; adjust ty2)
	    | adjust (AST.TupleTy tys) = List.app adjust tys
	  in
	    adjust ty
	  end

  (* destructively unify two types *)
    fun unify (ty1, ty2) = (case (TU.prune ty1, TU.prune ty2)
	   of (AST.ErrorTy, ty2) => true
	    | (ty1, AST.ErrorTy) => true
	    | (ty1 as AST.MetaTy mv1, ty2 as AST.MetaTy mv2) => (
		if MV.same(mv1, mv2) then ()
		else if MV.isDeeper(mv1, mv2)
		  then MV.instantiate(mv1, ty2)
		  else MV.instantiate(mv2, ty1);
		true)
	    | (AST.MetaTy mv1, ty2) => unifyWithMV (ty2, mv1)
	    | (ty1, AST.MetaTy mv2) => unifyWithMV (ty1, mv2)
	    | (AST.ConTy(tys1, tyc1), AST.ConTy(tys2, tyc2)) =>
		(TyCon.same(tyc1, tyc2)) andalso ListPair.allEq unify (tys1, tys2)
	    | (AST.FunTy(ty11, ty12), AST.FunTy(ty21, ty22)) =>
		unify(ty11, ty21) andalso unify(ty12, ty22)
	    | (AST.TupleTy tys1, AST.TupleTy tys2) =>
		ListPair.allEq unify (tys1, tys2)
	    | _ => false
	  (* end case *))

  (* unify a type with an uninstantiated meta-variable *)
    and unifyWithMV (ty, mv as AST.MVar{info=ref(AST.UNIV d), ...}) =
	  if (occursIn(mv, ty))
	    then false
	    else (adjustDepth(ty, d); MV.instantiate(mv, ty); true)
      | unifyWithMV _ = raise Fail "impossible"

  end
