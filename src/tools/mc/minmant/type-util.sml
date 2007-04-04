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
    val prune : AST.ty -> AST.ty

  (* apply a type variable to type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    val substitute : (AST.ty * (AST.tyvar * AST.ty) list) -> AST.ty

  (* instantiate a type scheme at the given lambda-nesting depth *)
    val instantiate : (int * AST.ty_scheme) -> AST.ty

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    val closeTy : (int * AST.ty) -> AST.ty_scheme

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types.
   *)
    val same : (AST.ty * AST.ty) -> bool

    val toString : AST.ty -> string

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    fun prune (AST.MetaTy(AST.MVar{info as ref(AST.INSTANCE ty), ...})) = let
	  val ty = prune ty
	  in
	    info := AST.INSTANCE ty;	(* path compression *)
	    ty
	  end
      | prune ty = ty

  (* apply a type variable to type substitution to a type *)
    fun applySubst (subst, ty) = let
	  fun inst ty = (case prune ty
		 of AST.ErrorTy => ty
		  | AST.MetaTy _ => raise Fail "unexpected meta variable"
		  | AST.VarTy tv => TVMap.lookup(subst, tv)
		  | AST.ConTy(args, tyc) => AST.ConTy(List.map inst args, tyc)
		  | AST.FunTy(ty1, ty2) => AST.FunTy(inst ty1, inst ty2)
		  | AST.TupleTy tys => AST.TupleTy(List.map inst tys)
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
    fun instantiate (_, AST.TyScheme([], ty)) = ty
      | instantiate (depth, AST.TyScheme(tvs, ty)) = let
	(* create a substitution from type variables to fresh meta variables *)
	  val subst = List.foldl
		(fn (tv, s) => TVMap.insert(s, tv, AST.MetaTy(MV.new depth)))
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
		 of AST.ErrorTy => (env, AST.ErrorTy)
		  | ty as AST.MetaTy(mv as AST.MVar{info=ref(AST.UNIV d), ...}) =>
		      if (d > depth)
			then (case MVMap.find(env, mv) (* generic variable *)
			   of SOME tv => (env, AST.VarTy tv)
			    | NONE => let
				val tv = newVar()
				in
				  (MVMap.insert(env, mv, tv), AST.VarTy tv)
				end
			  (* end case *))
			else (env, ty) (* non-generic variable *)
		  | AST.MetaTy _ => raise Fail "impossible"
		  | AST.VarTy _ => raise Fail "unexpected type variable"
		  | AST.ConTy(args, tyc) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, AST.ConTy(tys, tyc))
		      end
		  | AST.FunTy(ty1, ty2) => let
		      val (env, ty1) = genVars (ty1, env)
		      val (env, ty2) = genVars (ty2, env)
		      in
			(env, AST.FunTy(ty1, ty2))
		      end
		  | AST.TupleTy tys => let
		      val (env, tys) = genVarsForTys (tys, env)
		      in
			(env, AST.TupleTy tys)
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
	    AST.TyScheme(MVMap.listItems tvs, ty)
	  end

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types and allows ErrorTy to equal any type.
   *)
    fun same (ty1, ty2) = (case (prune ty1, prune ty2)
	   of (AST.ErrorTy, _) => true
	    | (_, AST.ErrorTy) => true
	    | (AST.MetaTy mv1, AST.MetaTy mv2) => MV.same(mv1, mv2)
	    | (AST.VarTy tv1, AST.VarTy tv2) => TyVar.same(tv1, tv2)
	    | (AST.ConTy(args1, tyc1), AST.ConTy(args2, tyc2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
	    | (AST.FunTy(ty11, ty12), AST.FunTy(ty21, ty22)) =>
		same(ty11, ty21) andalso same(ty21, ty22)
	    | (AST.TupleTy tys1, AST.TupleTy tys2) =>
		ListPair.allEq same (tys1, tys2)
	    | _ => false
	  (* end case *))

  (* return a string representation of a type (for debugging) *)
    fun toString (AST.ErrorTy) = "<error>"
      | toString (AST.MetaTy(AST.MVar{stamp, info})) = (case !info
	   of AST.UNIV d => concat["$", Stamp.toString stamp, "@", Int.toString d]
	    | AST.INSTANCE ty => (
		info := AST.UNIV(~1);
		concat["$", Stamp.toString stamp, " == ", toString ty]
		  before info := AST.INSTANCE ty)
	  (* end case *))
      | toString (AST.VarTy(AST.TVar{name, ...})) = Atom.toString name
      | toString (AST.ConTy([], tyc)) = Atom.toString(TyCon.nameOf tyc)
      | toString (AST.ConTy([ty], tyc)) = concat[
	    toString ty, " ", Atom.toString(TyCon.nameOf tyc)
	  ]
      | toString (AST.ConTy(tys, tyc)) = concat[
	    "(", String.concatWith "," (List.map toString tys), ")",
	    Atom.toString(TyCon.nameOf tyc)
	  ]
      | toString (AST.FunTy(ty1, ty2)) = concat[toString ty1, " -> ", toString ty2]
      | toString (AST.TupleTy tys) = "<tuplety>"

  end

