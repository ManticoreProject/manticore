(* var.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Var =
  struct
    local
      structure V = VarFn (
	struct
	  type kind = AST.var_kind
	  type ty = AST.ty_scheme
	  val defaultKind = AST.VK_None
	  val kindToString = AST.varKindToString
	  val tyToString = Types.toString
	end)
    in
    open V
    val newPoly = V.new
    fun new (name, ty) = newPoly (name, AST.TyScheme([], ty))
  (* close the type of the variable w.r.t. to the given lambda-nesting depth. *)
    fun closeTypeOf (depth, VarRep.V{ty as ref(AST.TyScheme(_, ty')), ...}) =
	  ty := TypeUtil.closeTy(depth, ty')
    end (* local *)
  end
