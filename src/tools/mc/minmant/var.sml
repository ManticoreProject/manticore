(* var.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Var : sig

    val new : Atom.atom * AST.ty -> AST.var
    val newPoly : Atom.atom * AST.ty_scheme -> AST.var

    val nameOf : AST.var -> Atom.atom

    val typeOf : AST.var -> AST.ty_scheme

  (* close the type of the variable w.r.t. to the given lambda-nesting depth. *)
    val closeTypeOf : int * AST.var -> unit

  end = struct

    fun newPoly (name, ty) = AST.Var{
	    stamp = Stamp.new(),
	    name = name,
	    ty = ref ty
	  }

    fun new (name, ty) = newPoly (name, AST.TyScheme([], ty))

    fun nameOf (AST.Var{name, ...}) = name

    fun typeOf (AST.Var{ty, ...}) = !ty

    fun closeTypeOf (depth, AST.Var{ty as ref(AST.TyScheme(_, ty')), ...}) =
	  ty := TypeUtil.closeTy(depth, ty')

  end
