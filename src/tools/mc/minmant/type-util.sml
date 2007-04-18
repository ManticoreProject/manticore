(* type-util.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Various utility functions for manipulating types.
 *)

structure TypeUtil : sig

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    val prune : Types.ty -> Types.ty

  (* apply a type variable to type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    val substitute : (Types.ty * (Types.tyvar * Types.ty) list) -> Types.ty

  (* instantiate a type scheme at the given lambda-nesting depth *)
    val instantiate : (int * Types.ty_scheme) -> Types.ty

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    val closeTy : (int * Types.ty) -> Types.ty_scheme

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types.
   *)
    val same : (Types.ty * Types.ty) -> bool

    val toString : Types.ty -> string

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    fun prune (Types.MetaTy(Types.MVar{info as ref(Types.INSTANCE ty), ...})) = let
	  val ty = prune ty
	  in
	    info := Types.INSTANCE ty;	(* path compression *)
	    ty
	  end
      | prune ty = ty

  (* apply a type variable to type substitution to a type *)
    fun applySubst (subst, ty) = let
	  fun inst ty = (case prune ty
		 of Types.ErrorTy => ty
		  | Types.MetaTy _ => raise Fail "unexpected meta variable"
		  | Types.VarTy tv => TVMap.lookup(subst, tv)
		  | Types.ConTy(args, tyc) => Types.ConTy(List.map inst args, tyc)
		  | Types.FunTy(ty1, ty2) => Types.FunTy(inst ty1, inst ty2)
		  | Types.TupleTy tys => Types.TupleTy(List.map inst tys)
		(* end case *))
	  in
	    inst ty
	  end

  (* apply a type-variable-to-type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    fun substitute (ty, []) = ty
      | substitute (ty, s) = applySubst (List.foldl TVMap.insert' TVMap.empty s, ty)

  (* instantiate a type scheme at the given lambda-nesting depth *)
    fun instantiate (_, Types.TyScheme([], ty)) = ty
      | instantiate (depth, Types.TyScheme(tvs, ty)) = let
	(* create a substitution from type variables to fresh meta variables *)
	  val subst = List.foldl
		(fn (tv, s) => TVMap.insert(s, tv, Types.MetaTy(MV.new depth)))
		  TVMap.empty tvs
	  in
	    applySubst (subst, ty)
	  end

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    fun closeTy (depth, ty) = let
	  val count = ref 0
	(* generate a fresh type variable *)
	  fun newVar () = let
		val id = !count
		in
		  count := id+1;
		  TyVar.new(Atom.atom("'M" ^ Int.toString id))
		end
	  fun genVars (ty, env) = (case prune ty
		 of Types.ErrorTy => (env, Types.ErrorTy)
		  | ty as Types.MetaTy(mv as Types.MVar{info=ref(Types.UNIV d), ...}) =>
		      if (d > depth)
			then (case MVMap.find(env, mv) (* generic variable *)
			   of SOME tv => (env, Types.VarTy tv)
			    | NONE => let
				val tv = newVar()
				in
				  (MVMap.insert(env, mv, tv), Types.VarTy tv)
				end
			  (* end case *))
			else (env, ty) (* non-generic variable *)
		  | Types.MetaTy _ => raise Fail "impossible"
		  | Types.VarTy _ => raise Fail "unexpected type variable"
		  | Types.ConTy(args, tyc) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, Types.ConTy(tys, tyc))
		      end
		  | Types.FunTy(ty1, ty2) => let
		      val (env, ty1) = genVars (ty1, env)
		      val (env, ty2) = genVars (ty2, env)
		      in
			(env, Types.FunTy(ty1, ty2))
		      end
		  | Types.TupleTy tys => let
		      val (env, tys) = genVarsForTys (tys, env)
		      in
			(env, Types.TupleTy tys)
		      end
		(* end case *))
	  and genVarsForTys (tys, env) = let
		fun f (ty, (env, tys)) = let
			val (env', ty') = genVars(ty, env)
			in
			  (env', ty'::tys)
			end
		in
		  List.foldr f (env, []) tys
		end
	  val (tvs, ty) = genVars (ty, MetaVar.Map.empty)
	  in
	    Types.TyScheme(MVMap.listItems tvs, ty)
	  end

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types and allows ErrorTy to equal any type.
   *)
    fun same (ty1, ty2) = (case (prune ty1, prune ty2)
	   of (Types.ErrorTy, _) => true
	    | (_, Types.ErrorTy) => true
	    | (Types.MetaTy mv1, Types.MetaTy mv2) => MV.same(mv1, mv2)
	    | (Types.VarTy tv1, Types.VarTy tv2) => TyVar.same(tv1, tv2)
	    | (Types.ConTy(args1, tyc1), Types.ConTy(args2, tyc2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
	    | (Types.FunTy(ty11, ty12), Types.FunTy(ty21, ty22)) =>
		same(ty11, ty21) andalso same(ty21, ty22)
	    | (Types.TupleTy tys1, Types.TupleTy tys2) =>
		ListPair.allEq same (tys1, tys2)
	    | _ => false
	  (* end case *))

  (* return a string representation of a type (for debugging) *)
    fun toString (Types.ErrorTy) = "<error>"
      | toString (Types.MetaTy(Types.MVar{stamp, info})) = (case !info
	   of Types.UNIV d => concat["$", Stamp.toString stamp, "@", Int.toString d]
	    | Types.INSTANCE ty => (
		info := Types.UNIV(~1);
		concat["$", Stamp.toString stamp, " == ", toString ty]
		  before info := Types.INSTANCE ty)
	  (* end case *))
      | toString (Types.VarTy(Types.TVar{name, ...})) = Atom.toString name
      | toString (Types.ConTy([], tyc)) = Atom.toString(TyCon.nameOf tyc)
      | toString (Types.ConTy([ty], tyc)) = concat[
	    toString ty, " ", Atom.toString(TyCon.nameOf tyc)
	  ]
      | toString (Types.ConTy(tys, tyc)) = concat[
	    "(", String.concatWith "," (List.map toString tys), ")",
	    Atom.toString(TyCon.nameOf tyc)
	  ]
      | toString (Types.FunTy(ty1, ty2)) = concat[toString ty1, " -> ", toString ty2]
      | toString (Types.TupleTy tys) = "<tuplety>"

  end

