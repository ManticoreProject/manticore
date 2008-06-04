(* bom-parse-tree.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of inline BOM code.
 *
 * TODO: add SML variable references
 *	 add Marks for error messages
 *)
 
 structure BOMParseTree =
   struct
   
    datatype raw_ty = datatype RawTypes.raw_ty

    type var = Atom.atom
    type offset = IntInf.int

    datatype defn
      = D_Extern of Atom.atom CFunctions.c_fun
      | D_TypeDef of Atom.atom * ty
      | D_Define of (bool * var * var_pat list * var_pat list * ty list option * exp option)

    and ty
      = T_Any				(* unknown type; uniform representation *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_Tuple of bool * ty list	(* heap-allocated tuple *)
      | T_Addr of ty
      | T_Fun of (ty list * ty list * ty list)
					(* function type; the second argument is the type of *)
					(* the exception continuation(s) *)
      | T_Cont of ty list		(* first-class continuation *)
      | T_CFun of CFunctions.c_proto	(* C functions *)
      | T_VProc				(* address of VProc runtime structure *)
      | T_TyCon of Atom.atom		(* high-level type constructor *)

    and exp
      = E_Let of (var_pat list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (simple_exp * exp * exp)
      | E_Case of (simple_exp * (pat * exp) list * (var_pat * exp) option)
      | E_Apply of (var * simple_exp list * simple_exp list)
      | E_Throw of (var * simple_exp list)
      | E_Return of simple_exp list
      | E_HLOpApply of (var * simple_exp list * simple_exp list)

    and rhs
      = RHS_Exp of exp
      | RHS_SimpleExp of simple_exp
      | RHS_Update of (int * simple_exp * simple_exp)
      | RHS_Promote of simple_exp			(* promote value to global heap *)
      | RHS_CCall of (var * simple_exp list)
      | RHS_VPStore of (offset * simple_exp * simple_exp)

    and simple_exp
      = SE_Var of var
      | SE_Alloc of simple_exp list
      | SE_Wrap of simple_exp				(* wrap raw value *)
      | SE_Select of (int * simple_exp)			(* select i'th field (zero-based) *)
      | SE_Unwrap of simple_exp				(* unwrap value *)
      | SE_AddrOf of (int * simple_exp)			(* address of i'th field (zero-based) *)
      | SE_Const of (Literal.literal * ty)
      | SE_MLString of string				(* ML string literal *)
      | SE_Cast of (ty * simple_exp)
      | SE_Prim of (Atom.atom * simple_exp list)	(* prim-op or data constructor *)
    (* VProc operations *)
      | SE_HostVProc					(* gets the hosting VProc *)
      | SE_VPLoad of (offset * simple_exp)

    and pat
      = P_DCon of (Atom.atom * var_pat list)
      | P_Const of (Literal.literal * ty)

    and var_pat
      = P_Wild of ty option
      | P_Var of (Atom.atom * ty)

    withtype lambda = (var * var_pat list * var_pat list * ty list * exp)

    type code = defn list

  end
