(* ft-var.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure FTVar = struct

  local
    structure V = VarFn (
      struct
        type kind = AST.var_kind
	type ty = FLAST.ty_scheme ref
	val defaultKind = AST.VK_None
	val kindToString = AST.varKindToString
	val tyToString = FTTypeUtil.schemeToString o !
      end)
  in

  open V

  fun newPoly (name, tyScheme) = V.new (name, ref tyScheme)

  fun new (name, ty) = newPoly (name, FLAST.TyScheme([], ty))

  fun newWithKind (name, k, t) = 
    V.newWithKind (name, k, ref (FLAST.TyScheme ([], t)))
	  
(* return the type scheme of a variable *)
  fun typeOf x = !(V.typeOf x)

(* return the type of a monomorphic variable *)
  fun monoTypeOf x = (case typeOf x
    of FLAST.TyScheme([], ty) => ty
     | tys => raise Fail "FIXME" (* TypeUtil.toMonoTy tys *)
   (* end case *))
		       
(* close the type of the variable w.r.t. to the given lambda-nesting depth. *)
  fun closeTypeOf (depth, v) = let
    val ty = V.typeOf v
    val FLAST.TyScheme(_, ty') = !ty
    in
      (* ty := TypeUtil.closeTy(depth, ty') *)
      raise Fail "FIXME"
    end

(* TODO: add setTypeOf *)

  end (* local *)

end
