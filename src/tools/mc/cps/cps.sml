(* cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPS =
  struct

    type ty = CPSTy.ty

    datatype exp
      = Let of (var list * rhs * exp)
      | Fun of (lambda list * exp)
      | Cont of (lambda * exp)
      | If of (var * exp * exp)
      | Switch of (var * (int * exp) list * exp option)
      | Apply of (var * var list)
      | Throw of (var * var list)

    and rhs
      = Var of var list
      | Literal of Literal.literal
      | Select of (int * var)		(* select i'th field (zero-based) *)
      | Alloc of var list
      | Wrap of var			(* wrap raw value *)
      | Unwrap of var			(* unwrap value *)
      | Prim of prim
      | CCall of (var * var list)

    and var_kind
      = VK_None
      | VK_Let of rhs
      | VK_Param of lambda

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and lambda = (var * var list * exp)

    datatype module = MODULE of lambda

    fun varKindToString VK_None = "None"
      | varKindToString (VK_Let _) = "Let"
      | varKindToString (VK_Param _) = "Param"

    structure Var = VarFn (
      struct
	type kind = var_kind
	type ty = ty
	val kindToString = varKindToString
	val tyToString = CPSTy.toString
      end)

  (* smart constructors; these enforce the variable kind invariant and should be
   * used to construct terms.
   *)
    fun mkLet (lhs, rhs, exp) = (
	  List.app (fn x => Var.setKind(x, VK_Let rhs)) lhs;
	  Let(lhs, rhs, exp))

  end
