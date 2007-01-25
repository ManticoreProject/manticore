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
      | Fix of (lambda list * exp)
      | If of (var * exp * exp)
      | Switch of (var * (int * exp) list * exp option)
      | Apply of (var * var list)

    and rhs
      = E_Var of var list
      | E_Literal of Literal.literal
      | E_Select of (int * var)		(* select i'th field (zero-based) *)
      | E_Alloc of var list
      | E_Wrap of var			(* wrap raw value *)
      | E_Unwrap of var			(* unwrap value *)
      | E_Prim of prim
      | E_CCall of (var * var list)

    and var_kind
      = VK_None
      | VK_Let of exp
      | VK_Param of lambda

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and lambda = (var * var list * exp)

    datatype module = MODULE of exp

  end
