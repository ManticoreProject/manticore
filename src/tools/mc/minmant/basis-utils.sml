(* basis-utils.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure BasisUtils : sig

    val forall : (AST.ty -> AST.ty) -> AST.ty_scheme
    val forallMulti : int * (AST.ty list -> AST.ty) -> AST.ty_scheme
    val monoVar : string * AST.ty -> AST.var
    val polyVar : string * (AST.ty -> AST.ty) -> AST.var
    val polyVarMulti : string * int * (AST.ty list -> AST.ty) -> AST.var

  end = struct

    structure A = AST

  (* forall : (A.ty -> A.ty) -> A.ty_scheme *)
    fun forall mkTy = let
        val tv = TyVar.new(Atom.atom "'a")
        in
          A.TyScheme([tv], mkTy(AST.VarTy tv))
        end

  (* forAllMulti : int * (A.ty list -> A.ty) -> A.ty_scheme *)
  (* Consume a number (of type variables) and a ty -> ty function. *)
  (* One must pass in the number of tyvars since there's no way to *)
  (* look inside mkTy to see how many type variables it expects. *)
    fun forallMulti (n, mkTy) = let
        fun mkTv n = TyVar.new(Atom.atom ("'a" ^ Int.toString n))
	val tvs = map mkTv (List.tabulate (n, (fn n => n)))
        in
          A.TyScheme(tvs, mkTy(map AST.VarTy tvs))
        end

  (* monoVar : string * A.ty -> A.var *)
    fun monoVar (name, ty) = Var.new(name, ty)

  (* polyVar : string * (A.ty -> A.ty) -> A.var *) 
    fun polyVar (name, mkTy) = Var.newPoly(name, forall mkTy)

  (* polyVarMulti : string * int * (A.ty list -> A.ty) -> A.var *)
  (* Consume a name, a number (of type variables), and a function to make a type. *)
  (* See forAllMulti above. *)
    fun polyVarMulti (name, n, mkTy) = Var.newPoly(name, forallMulti (n, mkTy))

  end
