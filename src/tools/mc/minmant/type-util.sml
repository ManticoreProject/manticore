(* type-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
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

  (* instantiate a type scheme at the given lambda-nesting depth.  This function
   * returns the list of type arguments (i.e., fresh metavariables allocated
   * for the bound variables of the scheme) and the resulting type.
   *)
    val instantiate : (int * Types.ty_scheme) -> (Types.ty list * Types.ty)

  (* instantiate a type scheme to a specific list of types.  We assume that
   * the types have the correct kinds for the corresponding type variables.
   *)
    val apply : (Types.ty_scheme * Types.ty list) -> Types.ty

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    val closeTy : (int * Types.ty) -> Types.ty_scheme

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types.
   *)
    val same : (Types.ty * Types.ty) -> bool

  (* convert various things to strings *)
    val tyvarToString : Types.tyvar -> string
    val toString : Types.ty -> string
    val schemeToString : Types.ty_scheme -> string

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map
    structure Ty = Types

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    fun prune (Ty.MetaTy(Ty.MVar{info as ref(Ty.INSTANCE ty), ...})) = let
	  val ty = prune ty
	  in
	    info := Ty.INSTANCE ty;	(* path compression *)
	    ty
	  end
      | prune (Ty.ClassTy(Ty.Class(info as ref(Ty.RESOLVED ty)))) = let
	  val ty = prune ty
	  in
	    info := Ty.RESOLVED ty;	(* path compression *)
	    ty
	  end
      | prune ty = ty

  (* apply a type variable to type substitution to a type *)
    fun applySubst (subst, ty) = let
	  fun inst ty = (case prune ty
		 of Ty.ErrorTy => ty
		  | Ty.MetaTy _ => raise Fail "unexpected meta variable"
		  | ty as Ty.ClassTy _ => ty
		  | Ty.VarTy tv => TVMap.lookup(subst, tv)
		  | Ty.ConTy(args, tyc) => Ty.ConTy(List.map inst args, tyc)
		  | Ty.FunTy(ty1, ty2) => Ty.FunTy(inst ty1, inst ty2)
		  | Ty.TupleTy tys => Ty.TupleTy(List.map inst tys)
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
    fun instantiate (_, Ty.TyScheme([], ty)) = ([], ty)
      | instantiate (depth, Ty.TyScheme(tvs, ty)) = let
	(* create a substitution from type variables to fresh meta variables *)
	  fun f (tv as AST.TVar{class,...}, (s, mvs)) = (case class
		   of NONE => let
			val mv = Ty.MetaTy(MV.new depth)
			in
			  (TVMap.insert(s, tv, mv), mv :: mvs)
			end
		    | SOME cls => let
			val cmv = TypeClass.new cls
			in
			  (TVMap.insert(s, tv, cmv), mvs)
			end
		  (* end case *))
	  val (subst, mvs) = List.foldl f (TVMap.empty, []) tvs
	  in
	    (mvs, applySubst (subst, ty))
	  end

  (* instantiate a type scheme to a specific list of types.  We assume that
   * the types have the correct kinds for the corresponding type variables.
   *)
    fun apply (Ty.TyScheme([], ty), []) = ty
      | apply (Ty.TyScheme(tvs, ty), tys) = let
	  fun ins (tv, ty, s) = TVMap.insert(s, tv, ty)
	  in
	    applySubst (ListPair.foldlEq ins TVMap.empty (tvs, tys), ty)
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
		 of Ty.ErrorTy => (env, Ty.ErrorTy)
		  | ty as Ty.MetaTy(mv as Ty.MVar{info=ref(Ty.UNIV d), ...}) =>
		      if (d > depth)
			then (case MVMap.find(env, mv) (* generic variable *)
			   of SOME tv => (env, Ty.VarTy tv)
			    | NONE => let
				val tv = newVar()
				in
				  (MVMap.insert(env, mv, tv), Ty.VarTy tv)
				end
			  (* end case *))
			else (env, ty) (* non-generic variable *)
		  | Ty.MetaTy _ => raise Fail "impossible"
		  | ty as Ty.ClassTy _ => (env, ty)
		  | Ty.VarTy _ => raise Fail "unexpected type variable"
		  | Ty.ConTy(args, tyc) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, Ty.ConTy(tys, tyc))
		      end
		  | Ty.FunTy(ty1, ty2) => let
		      val (env, ty1) = genVars (ty1, env)
		      val (env, ty2) = genVars (ty2, env)
		      in
			(env, Ty.FunTy(ty1, ty2))
		      end
		  | Ty.TupleTy tys => let
		      val (env, tys) = genVarsForTys (tys, env)
		      in
			(env, Ty.TupleTy tys)
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
	    Ty.TyScheme(MVMap.listItems tvs, ty)
	  end

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types and allows ErrorTy to equal any type.
   *)
    fun same (ty1, ty2) = (case (prune ty1, prune ty2)
	   of (Ty.ErrorTy, _) => true
	    | (_, Ty.ErrorTy) => true
	    | (Ty.MetaTy mv1, Ty.MetaTy mv2) => MV.same(mv1, mv2)
	    | (Ty.ClassTy(Ty.Class r1), Ty.ClassTy(Ty.Class r2)) => (r1 = r2)
	    | (Ty.VarTy tv1, Ty.VarTy tv2) => TyVar.same(tv1, tv2)
	    | (Ty.ConTy(args1, tyc1), Ty.ConTy(args2, tyc2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
	    | (Ty.FunTy(ty11, ty12), Ty.FunTy(ty21, ty22)) =>
		same(ty11, ty21) andalso same(ty21, ty22)
	    | (Ty.TupleTy tys1, Ty.TupleTy tys2) =>
		ListPair.allEq same (tys1, tys2)
	    | _ => false
	  (* end case *))

    fun tyvarToString (Ty.TVar{name, ...}) = Atom.toString name

  (* return a string representation of a type (for debugging) *)
    fun toString (Ty.ErrorTy) = "<error>"
      | toString (Ty.MetaTy(Ty.MVar{stamp, info})) = (case !info
	   of Ty.UNIV d => concat["$", Stamp.toString stamp, "@", Int.toString d]
	    | Ty.INSTANCE ty => (
		info := Ty.UNIV(~1);
		concat["$", Stamp.toString stamp, " == ", toString ty]
		  before info := Ty.INSTANCE ty)
	  (* end case *))
      | toString (Ty.ClassTy(Ty.Class(ref(Ty.CLASS cls)))) =
	  concat["<", TypeClass.toString cls, ">"]
      | toString (Ty.ClassTy(Ty.Class(ref(Ty.RESOLVED ty)))) = toString ty
      | toString (Ty.VarTy tv) = tyvarToString tv
      | toString (Ty.ConTy([], tyc)) = Atom.toString(TyCon.nameOf tyc)
      | toString (Ty.ConTy([ty], tyc)) = concat[
	    toString ty, " ", Atom.toString(TyCon.nameOf tyc)
	  ]
      | toString (Ty.ConTy(tys, tyc)) = concat[
	    "(", String.concatWith "," (List.map toString tys), ")",
	    Atom.toString(TyCon.nameOf tyc)
	  ]
      | toString (Ty.FunTy(ty1, ty2)) = concat[toString ty1, " -> ", toString ty2]
      | toString (Ty.TupleTy tys) = "<tuplety>"

  (* return the string representation of a type scheme *)
    fun schemeToString (Ty.TyScheme([], ty)) = toString ty
      | schemeToString (Ty.TyScheme(tvs, ty)) = concat[
	    "[", String.concatWith "," (List.map tyvarToString tvs), "]", toString ty
	  ]

  end

