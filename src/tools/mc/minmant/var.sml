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
	  type ty = AST.ty_scheme ref
	  val defaultKind = AST.VK_None
	  val kindToString = AST.varKindToString
	  val tyToString = TypeUtil.schemeToString o !
	end)
    in

    open V

    (* newPoly : string * Types.ty_scheme -> V.var *) 
    fun newPoly (name, tyScheme) = V.new (name, ref tyScheme)

    (* new : string * Types.ty -> V.var *)
    fun new (name, ty) = newPoly (name, AST.TyScheme([], ty))

    (* typeOf : V.var -> Types.ty *)
    fun typeOf x = !(V.typeOf x)

    (* closeTypeOf : int * VarRep.var_rep -> unit *)
    (* close the type of the variable w.r.t. to the given lambda-nesting depth. *)
    fun closeTypeOf (depth, VarRep.V{ty as ref(AST.TyScheme(_, ty')), ...}) =
	  ty := TypeUtil.closeTy(depth, ty')
    
    end (* local *)

  end
