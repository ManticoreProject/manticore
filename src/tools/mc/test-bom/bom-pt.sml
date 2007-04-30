(* bom-pt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of BOM.
 *)
 
 structure BOMPT =
   struct
   
    datatype raw_ty = datatype BOMTy.raw_ty
    datatype ty = datatype BOMTy.ty

    type var = Atom.atom
    type var_bind = var * ty

    datatype module = MODULE of {
	name : Atom.atom,
	externs : Atom.atom CFunctions.c_fun list,
	body : lambda
      }

    and exp
      = Let of (var_bind list * rhs * exp)
      | Fun of (lambda list * exp)
      | Cont of (lambda * exp)
      | If of (simple_exp * exp * exp)
      | Case of (simple_exp * (pat * exp) list * (var_pat * exp) option)
      | Apply of (var * simple_exp list * simple_exp list)
      | Throw of (var * simple_exp list)
      | Return of simple_exp list

    and rhs
      = Exp of exp
      | SimpleExp of simple_exp
      | Alloc of simple_exp list
      | Wrap of simple_exp			(* wrap raw value *)
      | CCall of (var * simple_exp list)

    and simple_exp
      = Var of var
      | Select of (int * simple_exp)	(* select i'th field (zero-based) *)
      | Literal of (Literal.literal * raw_ty option)
      | Cast of (ty * simple_exp)
      | Unwrap of simple_exp		(* unwrap value *)
      | Prim of (Atom.atom * simple_exp list)

    and pat
      = DConPat of (Atom.atom * var_pat list)
      | ConstPat of (Literal.literal * raw_ty option)

    and var_pat
      = WildPat
      | VarPat of (Atom.atom * ty)

    withtype lambda = (var * var_bind list * var_bind list * ty list * exp)

  end
