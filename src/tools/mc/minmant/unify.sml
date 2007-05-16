(* unify.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Unify : sig

  (* destructively unify two types; return true if successful, false otherwise *)
    val unify : Types.ty * Types.ty -> bool

  end = struct

    structure Ty = Types
    structure MV = MetaVar
    structure TU = TypeUtil
    structure TC = TypeClass

  (* does a meta-variable occur in a type? *)
    fun occursIn (mv, ty) = let
	  fun occurs ty = (case TU.prune ty
		 of Ty.ErrorTy => false
		  | (Ty.MetaTy mv') => MV.same(mv, mv')
		  | (Ty.ClassTy _) => false
		  | (Ty.VarTy _) => raise Fail "unexpected type variable"
		  | (Ty.ConTy(args, _)) => List.exists occurs args
		  | (Ty.FunTy(ty1, ty2)) => occurs ty1 orelse occurs ty2
		  | (Ty.TupleTy tys) => List.exists occurs tys
		(* end case *))
	  in
	    occurs ty
	  end

  (* adjust the depth of any non-instantiated meta-variable that is bound
   * deeper than the given depth.
   *)
    fun adjustDepth (ty, depth) = let
	  fun adjust Ty.ErrorTy = ()
	    | adjust (Ty.MetaTy(Ty.MVar{info as ref(Ty.UNIV d), ...})) =
		if (depth < d) then info := Ty.UNIV d else ()
	    | adjust (Ty.MetaTy(Ty.MVar{info=ref(Ty.INSTANCE ty), ...})) = adjust ty
	    | adjust (Ty.ClassTy(Ty.Class(ref(Ty.RESOLVED ty)))) = adjust ty
	    | adjust (Ty.ClassTy _) = ()
	    | adjust (Ty.VarTy _) = raise Fail "unexpected type variable"
	    | adjust (Ty.ConTy(args, _)) = List.app adjust args
	    | adjust (Ty.FunTy(ty1, ty2)) = (adjust ty1; adjust ty2)
	    | adjust (Ty.TupleTy tys) = List.app adjust tys
	  in
	    adjust ty
	  end

  (* destructively unify two types *)
    fun unify (ty1, ty2) = (case (TU.prune ty1, TU.prune ty2)
	   of (Ty.ErrorTy, ty2) => true
	    | (ty1, Ty.ErrorTy) => true
	    | (ty1 as Ty.MetaTy mv1, ty2 as Ty.MetaTy mv2) => (
		if MV.same(mv1, mv2) then ()
		else if MV.isDeeper(mv1, mv2)
		  then MV.instantiate(mv1, ty2)
		  else MV.instantiate(mv2, ty1);
		true)
	    | (Ty.MetaTy mv1, ty2) => unifyWithMV (ty2, mv1)
	    | (ty1, Ty.MetaTy mv2) => unifyWithMV (ty1, mv2)
	    | (ty1 as Ty.ClassTy cl1, ty2 as Ty.ClassTy cl2) => unifyClasses (cl1, cl2)
	    | (Ty.ClassTy cl1, ty2) => unifyWithClass (ty2, cl1)
	    | (ty1, Ty.ClassTy cl2) => unifyWithClass (ty1, cl2)
	    | (Ty.ConTy(tys1, tyc1), Ty.ConTy(tys2, tyc2)) =>
		(TyCon.same(tyc1, tyc2)) andalso ListPair.allEq unify (tys1, tys2)
	    | (Ty.FunTy(ty11, ty12), Ty.FunTy(ty21, ty22)) =>
		unify(ty11, ty21) andalso unify(ty12, ty22)
	    | (Ty.TupleTy tys1, Ty.TupleTy tys2) =>
		ListPair.allEq unify (tys1, tys2)
	    | _ => false
	  (* end case *))

  (* unify a type with an uninstantiated meta-variable *)
    and unifyWithMV (ty, mv as Ty.MVar{info=ref(Ty.UNIV d), ...}) =
	  if (occursIn(mv, ty))
	    then false
	    else (adjustDepth(ty, d); MV.instantiate(mv, ty); true)
      | unifyWithMV _ = raise Fail "impossible"

    and unifyClasses (Ty.Class(info1 as ref(Ty.CLASS cl1)), Ty.Class(info2 as ref(Ty.CLASS cl2))) =
	(case (cl1, cl2) of
	     (Ty.Int, Ty.Float) => false
	   | (Ty.Float, Ty.Int) => false
	   | (Ty.Int, _) => (info2 := Ty.CLASS Ty.Int; true)
	   | (_, Ty.Int) => (info1 := Ty.CLASS Ty.Int; true)
	   | (Ty.Float, _) => (info2 := Ty.CLASS Ty.Float; true)
	   | (_, Ty.Float) => (info1 := Ty.CLASS Ty.Float; true)
	   | (Ty.Num, _) => (info2 := Ty.CLASS Ty.Num; true)
	   | (_, Ty.Num) => (info1 := Ty.CLASS Ty.Num; true)
	   | (Ty.Order, _) => (info2 := Ty.CLASS Ty.Order; true)
	   | (_, Ty.Order) => (info1 := Ty.CLASS Ty.Order; true)
	   | _ => true
	(* end case *))
      | unifyClasses _ = raise Fail "impossible"

    and unifyWithClass (ty, Ty.Class (info as ref(Ty.CLASS cl))) =
	if (case cl of
		Ty.Int => TC.isClass (ty, TC.IntClass)
	      | Ty.Float => TC.isClass (ty, TC.FloatClass)
	      | Ty.Num => TC.isClass (ty, TC.NumClass)
	      | Ty.Order => TC.isClass (ty, TC.OrderClass)
	      | Ty.Eq => TC.isEqualityType ty
	   (* end case *))
	then (info := Ty.RESOLVED ty; true)
	else false
  end
