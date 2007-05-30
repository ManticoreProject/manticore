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

    local
	val mv_changes = ref []
	val cv_changes = ref []

	fun assign_mv (mv as Types.MVar {info, ...}, ty, reconstruct) =
	    (if reconstruct
	     then mv_changes := (info, !info) :: mv_changes
	     else ();
	     MV.instantiate (mv, ty))

	fun assign_cl (cl as Types.Class info, tycl, reconstruct) =
	    (if reconstruct
	     then cv_changes := (info, !info) :: cv_changes
	     else ();
	     info := tycl)

	(* unify two types *)
	fun unifyRC (ty1, ty2, reconstruct) =
	    (case (TU.prune ty1, TU.prune ty2)
	      of (Ty.ErrorTy, ty2) => true
	       | (ty1, Ty.ErrorTy) => true
	       | (ty1 as Ty.MetaTy mv1, ty2 as Ty.MetaTy mv2) => (
		 if MV.same(mv1, mv2) then ()
		 else if MV.isDeeper(mv1, mv2)
		 then assign_mv (mv1, ty2, reconstruct)
		 else assign_mv (mv2, ty1, reconstruct);
		 true)
	       | (Ty.MetaTy mv1, ty2) => unifyWithMV (ty2, mv1, reconstruct)
	       | (ty1, Ty.MetaTy mv2) => unifyWithMV (ty1, mv2, reconstruct)
	       | (ty1 as Ty.ClassTy cl1, ty2 as Ty.ClassTy cl2) => unifyClasses (cl1, cl2, reconstruct)
	       | (Ty.ClassTy cl1, ty2) => unifyWithClass (ty2, cl1, reconstruct)
	       | (ty1, Ty.ClassTy cl2) => unifyWithClass (ty1, cl2, reconstruct)
	       | (Ty.ConTy(tys1, tyc1), Ty.ConTy(tys2, tyc2)) =>
		 (TyCon.same(tyc1, tyc2)) andalso ListPair.allEq unifyRC (tys1, tys2, reocnstruct)
	       | (Ty.FunTy(ty11, ty12), Ty.FunTy(ty21, ty22)) =>
		 unifyRC(ty11, ty21, reconstruct) andalso unifyRC(ty12, ty22, reconstruct)
	       | (Ty.TupleTy tys1, Ty.TupleTy tys2) =>
		 ListPair.allEq unifyRC (tys1, tys2, reconstruct)
	       | _ => false
	    (* end case *))
			       
	(* unify a type with an uninstantiated meta-variable *)
	and unifyWithMV (ty, mv as Ty.MVar{info=ref(Ty.UNIV d), ...}, rc) =
	    if (occursIn(mv, ty))
	    then false
	    else (adjustDepth(ty, d); assign_mv(mv, ty, rc); true)
	  | unifyWithMV _ = raise Fail "impossible"
				  
	and unifyClasses (c1 as Ty.Class(info1 as ref(Ty.CLASS cl1)), c2 as Ty.Class(info2 as ref(Ty.CLASS cl2)), rc) =
	    (case (cl1, cl2) of
		 (Ty.Int, Ty.Float) => false
	       | (Ty.Float, Ty.Int) => false
	       | (Ty.Int, _) => (assign_cl (c2, Ty.CLASS Ty.Int, rc); true)
	       | (_, Ty.Int) => (assign_cl (c1, Ty.CLASS Ty.Int, rc); true)
	       | (Ty.Float, _) => (assign_cl (c2, Ty.CLASS Ty.Float, rc); true)
	       | (_, Ty.Float) => (assign_cl (c1, Ty.CLASS Ty.Float, rc); true)
	       | (Ty.Num, _) => (assign_cl (c2, Ty.CLASS Ty.Num, rc); true)
	       | (_, Ty.Num) => (assign_cl (c1, Ty.CLASS Ty.Num, rc); true)
	       | (Ty.Order, _) => (assign_cl (c2, Ty.CLASS Ty.Order, rc); true)
	       | (_, Ty.Order) => (assign_cl (c1, Ty.CLASS Ty.Order, rc); true)
	       | _ => true
	    (* end case *))
	  | unifyClasses _ = raise Fail "impossible"
				   
	and unifyWithClass (ty, c as Ty.Class (info as ref(Ty.CLASS cl)), rc) =
	    if (case cl of
		    Ty.Int => TC.isClass (ty, TC.IntClass)
		  | Ty.Float => TC.isClass (ty, TC.FloatClass)
		  | Ty.Num => TC.isClass (ty, TC.NumClass)
		  | Ty.Order => TC.isClass (ty, TC.OrderClass)
		  | Ty.Eq => TC.isEqualityType ty
	       (* end case *))
	    then (assign_cl (c, Ty.RESOLVED ty, rc); true)
	    else false
    in
    fun unify (ty1, ty2) = unifyRC (ty1, ty2, false)
			   
    fun unifiable (ty1, ty2) =
	(mv_changes := [];
	 cv_changes := [];
	 let
	     val ret = unifyRC (ty1, ty2, true)
	 in
	     List.app (op :=) mv_changes;
	     List.app (op :=) cv_changes;
	     ret
	 end)
    end
end
