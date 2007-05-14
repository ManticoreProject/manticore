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

    datatype ty
      = T_Any				(* unknown type; uniform representation *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_Wrap of raw_ty		(* boxed raw value *)
      | T_Tuple of bool * ty list	(* heap-allocated tuple *)
      | T_Addr of ty
      | T_Fun of (ty list * ty list * ty list)
					(* function type; the second argument is the type of *)
					(* the exception continuation(s) *)
      | T_Cont of ty list		(* first-class continuation *)
      | T_CFun of CFunctions.c_proto	(* C functions *)
      | T_VProc				(* address of VProc runtime structure *)
      | T_TyCon of Atom.atom		(* high-level type constructor *)

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
      | HLOpApply of (var * simple_exp list * simple_exp list)

    and rhs
      = Exp of exp
      | SimpleExp of simple_exp
      | Update of (int * simple_exp * simple_exp)
      | Alloc of simple_exp list
      | Wrap of simple_exp			(* wrap raw value *)
      | CCall of (var * simple_exp list)

    and simple_exp
      = Var of var
      | Select of (int * simple_exp)		(* select i'th field (zero-based) *)
      | AddrOf of (int * simple_exp)		(* address of i'th field (zero-based) *)
      | Const of (Literal.literal * ty)
      | Cast of (ty * simple_exp)
      | Unwrap of simple_exp			(* unwrap value *)
      | Prim of (Atom.atom * simple_exp list)	(* prim-op or data constructor *)
    (* VProc operations *)
      | HostVProc				(* gets the hosting VProc *)

    and pat
      = DConPat of (Atom.atom * var_pat list)
      | ConstPat of (Literal.literal * ty)

    and var_pat
      = WildPat
      | VarPat of (Atom.atom * ty)

    withtype lambda = (var * var_bind list * var_bind list * ty list * exp)

  end
