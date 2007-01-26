(* cps-pt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of CPS.
 *)

structure CPSPT =
  struct

    datatype raw_ty = datatype CPSTy.raw_ty
    datatype ty = datatype CPSTy.ty

    type var = Atom.atom
    type var_bind = var * ty

    datatype module = MODULE of lambda

    and exp
      = Let of (var_bind list * rhs * exp)
      | Fun of (lambda list * exp)
      | Cont of (lambda * exp)
      | If of (simple_exp * exp * exp)
      | Switch of (simple_exp * (int * exp) list * exp option)
      | Apply of (var * simple_exp list)
      | Throw of (var * simple_exp list)

    and rhs
      = SimpleExp of simple_exp
      | Alloc of simple_exp list
      | Wrap of simple_exp			(* wrap raw value *)
      | CCall of (var * simple_exp list)

    and simple_exp
      = Var of var
      | Select of (int * simple_exp)	(* select i'th field (zero-based) *)
      | Literal of Literal.literal
      | Unwrap of simple_exp		(* unwrap value *)
      | Prim of (Atom.atom * simple_exp list)

    withtype lambda = (var * var_bind list * exp)

  end
