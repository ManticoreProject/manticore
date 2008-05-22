(* match-ty.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Match two types.
 *)

structure MatchTy : sig

  (* nondestructively check if two type schemes are unifiable. the left-hand type scheme
   * comes from the specification and the right comes from the module.
   *)
    val match : (Types.ty_scheme * Types.ty_scheme) -> bool

  (* same as above, except for types *)
    val matchTys : (Types.ty * Types.ty) -> bool

  end = struct

    structure Ty = Types
    structure MV = MetaVar
    structure TU = TypeUtil
    structure TC = TypeClass
    structure Env = ModuleEnv

  (* context for unification *)
    datatype ctx
      = CTX of {
	     tvMatches    : AST.tyvar TyVar.Map.map ref                (* matching type variables *)
           }

    fun matchTyVars (CTX{tvMatches, ...}, sigTv, modTv) = let
          val tvMatches' = !tvMatches
          in 
             case TyVar.Map.find(tvMatches', sigTv)
	      of NONE => (
		 tvMatches := TyVar.Map.insert(tvMatches', sigTv, modTv); 
		 true)
	       | SOME tv => TyVar.same(tv, modTv)
          end

(* FIXME: add a control to enable this flag *)
    val debugMatch = ref false

    fun getRealizationTy (ty as Ty.ConTy(tys, tyc)) = (case ModuleEnv.getRealizationOfTyc tyc
          of NONE => ty
	   | SOME (ModuleEnv.TyDef (Ty.TyScheme(_, ty))) => ty
	   | SOME (ModuleEnv.TyCon tyc) => Ty.ConTy(tys, tyc)
          (* end case *))
      | getRealizationTy ty = ty

  (* does a meta-variable occur in a type? *)
    fun occursIn (mv, ty) = let
	  fun occurs ty = (case TU.prune ty
		 of Ty.ErrorTy => false
		  | (Ty.MetaTy mv') => MV.same(mv, mv')
		  | (Ty.VarTy _) => false
		  | (Ty.ConTy(args, _)) => List.exists occurs args
		  | (Ty.FunTy(ty1, ty2)) => occurs ty1 orelse occurs ty2
		  | (Ty.TupleTy tys) => List.exists occurs tys
		(* end case *))
	  in
	    occurs ty
	  end

  (* match two types *)
    fun matchRC (ctx, ty1, ty2, reconstruct) = let
	  val mv_changes = ref []
	  fun assignMV (info, newInfo) = (
		if reconstruct
		  then mv_changes := (info, !info) :: !mv_changes
		  else ();
		info := newInfo)
	(* adjust the depth of any non-instantiated meta-variable that is bound
	 * deeper than the given depth.
	 *)
	  fun adjustDepth (ty, depth) = let
		fun adjust Ty.ErrorTy = ()
		  | adjust (Ty.MetaTy(Ty.MVar{info, ...})) = (case !info
		       of Ty.UNIV d => if (depth < d) then assignMV(info, Ty.UNIV depth) else ()
			| Ty.CLASS _ => ()
			| Ty.INSTANCE ty => adjust ty
		      (* end case *))
		  | adjust (Ty.VarTy _) = ()
		  | adjust (Ty.ConTy(args, _)) = List.app adjust args
		  | adjust (Ty.FunTy(ty1, ty2)) = (adjust ty1; adjust ty2)
		  | adjust (Ty.TupleTy tys) = List.app adjust tys
		in
		  adjust ty
		end
	  fun mtch (ty1, ty2) = (case (getRealizationTy (TU.prune ty1), TU.prune ty2)
		 of (Ty.ErrorTy, ty2) => true
		  | (ty1, Ty.ErrorTy) => true
		  | (Ty.VarTy tv1, Ty.VarTy tv2) => 
		      matchTyVars(ctx, tv1, tv2)
		  | (ty, Ty.VarTy tv2) => true
		  | (ty1, Ty.MetaTy mv2) => matchWithMV (ty1, mv2)
		  | (Ty.ConTy(tys1, tyc1), Ty.ConTy(tys2, tyc2)) => 
		       TyCon.same(tyc1, tyc2) andalso ListPair.allEq mtch (tys1, tys2)
		  | (Ty.FunTy(ty11, ty12), Ty.FunTy(ty21, ty22)) => 
		      mtch(ty11, ty21) andalso mtch(ty12, ty22)
		  | (Ty.TupleTy tys1, Ty.TupleTy tys2) =>
		      ListPair.allEq mtch (tys1, tys2)
		  | _ => false
	       (* end case *))
	(* match a type with an uninstantiated meta-variable *)
	  and matchWithMV (ty, mv as Ty.MVar{info, ...}) = let
		fun isClass cls =  if TC.isClass(ty, cls)
                       then (assignMV(info, Ty.INSTANCE ty); true)
		       else false
		in
		  case !info
		   of Ty.UNIV d => if (occursIn(mv, ty))
			then false
			else (adjustDepth(ty, d); assignMV(info, Ty.INSTANCE ty); true)
		    | Ty.CLASS cls => if occursIn(mv, ty)
			then false
			else (case cls
			   of Ty.Int => isClass Basis.IntClass
			    | Ty.Float => isClass Basis.FloatClass
			    | Ty.Num => isClass Basis.NumClass
			    | Ty.Order => isClass Basis.OrderClass
			    | Ty.Eq => if TC.isEqualityType ty
				then (assignMV(info, Ty.INSTANCE ty); true)
				else false
			  (* end case *))
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  and matchMV (mv1 as Ty.MVar{info=info1, ...}, mv2 as Ty.MVar{info=info2, ...}) = let
		fun assign (info1, mv2) = (assignMV(info1, Ty.INSTANCE(Ty.MetaTy mv2)); true)
		in
		  case (!info1, !info2)
		   of (Ty.UNIV d1, Ty.UNIV d2) => if (d1 < d2)
			then assign(info2, mv1)
			else assign(info1, mv2)
		    | (Ty.UNIV _, _) => assign(info1, mv2)
		    | (_, Ty.UNIV _) => assign(info2, mv1)
		    | (Ty.CLASS cl1, Ty.CLASS cl2) => (case (cl1, cl2)
			 of (Ty.Int, Ty.Float) => false
			  | (Ty.Float, Ty.Int) => false
			  | (Ty.Int, _) => assign (info2, mv1)
			  | (_, Ty.Int) => assign (info1, mv2)
			  | (Ty.Float, _) => assign (info2, mv1)
			  | (_, Ty.Float) => assign (info1, mv2)
			  | (Ty.Num, _) => assign (info2, mv1)
			  | (_, Ty.Num) => assign (info1, mv2)
			  | (Ty.Order, _) => assign (info2, mv1)
			  | (_, Ty.Order) => assign (info1, mv2)
			  | _ => true
			(* end case *))
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	  val ty = mtch (ty1, ty2)
	  in
	    if reconstruct
	      then List.app (op :=) (!mv_changes)
	      else ();
	    ty
	  end

    fun match (ty1, ty2) = let
	  val _ = if !debugMatch
		    then print(concat[
			"match (", TypeUtil.fmt {long=true} ty1, ", ",
			TypeUtil.fmt {long=true} ty2, ")\n"
		      ])
		    else ()
	  val res = matchRC (CTX{tvMatches=ref TyVar.Map.empty}, ty1, ty2, false)
	  in
	    if !debugMatch
	      then if res
		then print(concat["  = ", TypeUtil.fmt {long=true} ty1, "\n"])
		else print "  FAILURE\n"
	      else ();
	    res
	  end

    fun matchTys (ty1, ty2) = 
	   matchRC (CTX{tvMatches=ref TyVar.Map.empty}, ty1, ty2, true)

    fun match (Ty.TyScheme (tvs1, ty1), Ty.TyScheme (tvs2, ty2)) = let
	val ctx = CTX{tvMatches=ref TyVar.Map.empty}
        in
	   matchRC(ctx, ty1, ty2, true)
	end
			   
  end
