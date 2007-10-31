(* hlop-def-pt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of a high-level operator definition file.
 *)
 
 structure HLOpDefPT =
   struct
   
    datatype raw_ty = datatype BOMTyPT.raw_ty
    datatype ty = datatype BOMTyPT.ty

    type var = Atom.atom
    type offset = IntInf.int

    datatype file = FILE of defn list

    and defn
      = Extern of Atom.atom CFunctions.c_fun
      | TypeDef of Atom.atom * ty
      | Define of (bool * var * var_pat list * var_pat list * ty list option * exp option)
      | RWImport of Atom.atom list

    and exp
      = Let of (var_pat list * rhs * exp)
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
      | GAlloc of simple_exp list
      | Promote of simple_exp			(* promote value to global heap *)
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
      | VPLoad of (offset * simple_exp)
      | VPStore of (offset * simple_exp * simple_exp)

    and pat
      = DConPat of (Atom.atom * var_pat list)
      | ConstPat of (Literal.literal * ty)

    and var_pat
      = WildPat of ty option
      | VarPat of (Atom.atom * ty)

    withtype lambda = (var * var_pat list * var_pat list * ty list * exp)

  end
