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

  (* nondestructively check if two types are unifiable. *)
    val unifiable : Types.ty * Types.ty -> bool

  end = struct

    structure Ty = Types
    structure MV = MetaVar
    structure TU = TypeUtil
    structure TC = TypeClass

(* FIXME: add a control to enable this flag *)
    val debugUnify = ref false

  (* does a meta-variable occur in a type? *)
    fun occursIn (mv, ty) = let
	  fun occurs ty = (case TU.prune ty
		 of Ty.ErrorTy => false
		  | (Ty.MetaTy mv') => MV.same(mv, mv')
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
	    | adjust (Ty.MetaTy(Ty.MVar{info, ...})) = (case !info
		 of Ty.UNIV d => if (depth < d) then info := Ty.UNIV d else ()
		  | Ty.CLASS _ => ()
		  | Ty.INSTANCE ty => adjust ty
		(* end case *))
	    | adjust (Ty.VarTy _) = raise Fail "unexpected type variable"
	    | adjust (Ty.ConTy(args, _)) = List.app adjust args
	    | adjust (Ty.FunTy(ty1, ty2)) = (adjust ty1; adjust ty2)
	    | adjust (Ty.TupleTy tys) = List.app adjust tys
	  in
	    adjust ty
	  end

  (* unify two types *)
    fun unifyRC (ty1, ty2, reconstruct) = let
	  val mv_changes = ref []
	  fun assignMV (mv as Types.MVar {info, ...}, ty) = (
		if reconstruct
		  then mv_changes := (info, !info) :: !mv_changes
		  else ();
		MV.instantiate (mv, ty))
	  fun uni (ty1, ty2) = (case (TU.prune ty1, TU.prune ty2)
		 of (Ty.ErrorTy, ty2) => true
		  | (ty1, Ty.ErrorTy) => true
		  | (ty1 as Ty.MetaTy mv1, ty2 as Ty.MetaTy mv2) =>
		      MetaVar.same(mv1, mv2) orelse unifyMV(mv1, mv2)
		  | (Ty.MetaTy mv1, ty2) => unifyWithMV (ty2, mv1)
		  | (ty1, Ty.MetaTy mv2) => unifyWithMV (ty1, mv2)
		  | (Ty.ConTy(tys1, tyc1), Ty.ConTy(tys2, tyc2)) =>
		    (TyCon.same(tyc1, tyc2)) andalso ListPair.allEq uni (tys1, tys2)
		  | (Ty.FunTy(ty11, ty12), Ty.FunTy(ty21, ty22)) =>
		      uni(ty11, ty21) andalso uni(ty12, ty22)
		  | (Ty.TupleTy tys1, Ty.TupleTy tys2) =>
		      ListPair.allEq uni (tys1, tys2)
		  | _ => false
	       (* end case *))
	(* unify a type with an uninstantiated meta-variable *)
	  and unifyWithMV (ty, mv as Ty.MVar{info, ...}) = let
		fun isClass cls = if TC.isClass(ty, cls)
		      then (assignMV(mv, ty); true)
		      else false
		in
		  case !info
		   of Ty.UNIV d => if (occursIn(mv, ty))
			then false
			else (adjustDepth(ty, d); assignMV(mv, ty); true)
		    | Ty.CLASS cls => if occursIn(mv, ty)
			then false
			else (case cls
			   of Ty.Int => isClass Basis.IntClass
			    | Ty.Float => isClass Basis.FloatClass
			    | Ty.Num => isClass Basis.NumClass
			    | Ty.Order => isClass Basis.OrderClass
			    | Ty.Eq => TC.isEqualityType ty
			  (* end case *))
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  and unifyMV (mv1 as Ty.MVar{info=info1, ...}, mv2 as Ty.MVar{info=info2, ...}) = let
		fun assign (mv1, mv2) = (assignMV(mv1, Ty.MetaTy mv2); true)
		in
		  case (!info1, !info2)
		   of (Ty.UNIV d1, Ty.UNIV d2) => if (d1 < d2)
			then assign(mv2, mv1)
			else assign(mv1, mv2)
		    | (Ty.UNIV _, _) => assign(mv1, mv2)
		    | (_, Ty.UNIV _) => assign(mv2, mv1)
		    | (Ty.CLASS cl1, Ty.CLASS cl2) => (case (cl1, cl2)
			 of (Ty.Int, Ty.Float) => false
			  | (Ty.Float, Ty.Int) => false
			  | (Ty.Int, _) => assign (mv2, mv1)
			  | (_, Ty.Int) => assign (mv1, mv2)
			  | (Ty.Float, _) => assign (mv2, mv1)
			  | (_, Ty.Float) => assign (mv1, mv2)
			  | (Ty.Num, _) => assign (mv2, mv1)
			  | (_, Ty.Num) => assign (mv1, mv2)
			  | (Ty.Order, _) => assign (mv2, mv1)
			  | (_, Ty.Order) => assign (mv1, mv2)
			  | _ => true
			(* end case *))
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  val ty = uni (ty1, ty2)
	  in
	    if reconstruct
	      then List.app (op :=) (!mv_changes)
	      else ();
	    ty
	  end

    fun unify (ty1, ty2) = let
	  val _ = if !debugUnify
		    then print(concat[
			"unify (", TypeUtil.fmt {long=true} ty1, ", ",
			TypeUtil.fmt {long=true} ty2, ")\n"
		      ])
		    else ()
	  val res = unifyRC (ty1, ty2, false)
	  in
	    if !debugUnify
	      then if res
		then print(concat["  = ", TypeUtil.fmt {long=true} ty1, "\n"])
		else print "  FAILURE\n"
	      else ();
	    res
	  end
    fun unifiable (ty1, ty2) = unifyRC (ty1, ty2, true)
			   
  end
