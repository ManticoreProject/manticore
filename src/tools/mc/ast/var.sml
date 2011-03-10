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

    fun newPoly (name, tyScheme) = V.new (name, ref tyScheme)

    fun new (name, ty) = newPoly (name, AST.TyScheme([], ty))

    fun newWithKind (name, k, t) = 
	  V.newWithKind (name, k, ref (AST.TyScheme ([], t)))

  (* return the type scheme of a variable *)
    fun typeOf x = !(V.typeOf x)

  (* return the type of a monomorphic variable *)
    fun monoTypeOf x = (case typeOf x
	   of AST.TyScheme([], ty) => ty
	    | tys => TypeUtil.toMonoTy tys
	  (* end case *))

  (* close the type of the variable w.r.t. to the given lambda-nesting depth. *)
    fun closeTypeOf (depth, v) = let
	  val ty = V.typeOf v
	  val AST.TyScheme(_, ty') = !ty
	  in
	    ty := TypeUtil.closeTy(depth, ty')
	  end

  (* interface types for variables post flattening transformation *)
    local
      val {getFn : var -> Types.ty_scheme option, setFn, ...} = 
        V.newProp (fn _ => NONE)
    in
      fun setInterfaceTy (x, t) = setFn (x, SOME t)
      fun getInterfaceTy x = getFn x    	  
    end

  (* TODO: add setTypeOf *)

    end (* local *)

  end
