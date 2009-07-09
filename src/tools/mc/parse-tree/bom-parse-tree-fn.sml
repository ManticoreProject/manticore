 (* bom-parse-tree.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of inline BOM code.
 *
 *)
 
 functor BOMParseTreeFn (
    type var_use
    type var_bind
    type pml_var
    type ty_con
    type ty_def
    type prim
    type dcon
    type hlop_use
    type hlop_bind
    type c_id
  ) = struct
   
    datatype raw_ty = datatype RawTypes.raw_ty

    type var_use = var_use
    type var_bind = var_bind
    type pml_var = pml_var
    type ty_con = ty_con
    type ty_def = ty_def
    type cond = Atom.atom
    type prim = prim
    type dcon = dcon
    type hlop_use = hlop_use
    type hlop_bind = hlop_bind
    type c_id = c_id

  (* a term marked with a source-map span *)
    type 'a mark = 'a Error.mark

    type offset = IntInf.int

    datatype defn
      = D_Mark of defn mark
      | D_Extern of c_id CFunctions.c_fun          (* foreign function prototype *)
      | D_TypeDef of ty_def * ty                   (* type definition *)
      | D_Define of (bool * hlop_bind * var_pat list * var_pat list * ty list option * exp option)
		                                   (* HLOp *)
      | D_ImportML of (bool * hlop_bind * pml_var) (* form to import an ML function *)
      | D_Rewrite of { label  : Atom.atom,         (* hlop rewrite rule *)
                       lhs    : rw_pattern,
                       rhs    : rw_pattern,
                       weight : IntInf.int }

    and ty
      = T_Mark of ty mark
      | T_Any				(* unknown type; uniform representation *)
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
      | T_TyCon of ty_con		(* high-level type constructor *)

    and exp
      = E_Mark of exp mark
      | E_Let of (var_pat list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (cond * simple_exp list * exp * exp)
      | E_Case of (simple_exp * (pat * exp) list * (var_pat * exp) option)
      | E_Apply of (var_use * simple_exp list * simple_exp list)
      | E_Throw of (var_use * simple_exp list)
      | E_Return of simple_exp list
      | E_HLOpApply of (hlop_use * simple_exp list * simple_exp list)

    and rhs
      = RHS_Mark of rhs mark
      | RHS_Exp of exp
      | RHS_SimpleExp of simple_exp
      | RHS_Update of (int * simple_exp * simple_exp)
      | RHS_Promote of simple_exp			(* promote value to global heap *)
      | RHS_CCall of (c_id * simple_exp list)
      | RHS_VPStore of (offset * simple_exp * simple_exp)

    and simple_exp
      = SE_Mark of simple_exp mark
      | SE_Var of var_use
      | SE_Alloc of simple_exp list
      | SE_Wrap of simple_exp				(* wrap raw value *)
      | SE_Select of (int * simple_exp)			(* select i'th field (zero-based) *)
      | SE_Unwrap of simple_exp				(* unwrap value *)
      | SE_AddrOf of (int * simple_exp)			(* address of i'th field (zero-based) *)
      | SE_Const of (Literal.literal * ty)
      | SE_MLString of string				(* ML string literal *)
      | SE_Cast of (ty * simple_exp)
      | SE_Prim of (prim * simple_exp list)		(* prim-op or data constructor *)
    (* VProc operations *)
      | SE_HostVProc					(* gets the hosting VProc *)
      | SE_VPLoad of (offset * simple_exp)
      | SE_VPAddr of (offset * simple_exp)		(* address of byte offset in VProc *)

    and pat
      = P_PMark of pat mark
      | P_DCon of (dcon * var_pat list)
      | P_Const of (Literal.literal * ty)

    and var_pat
      = P_VPMark of var_pat mark
      | P_Wild of ty option
      | P_Var of (var_bind * ty)

  (* rewrite pattern *)
    and rw_pattern = 
	     RW_Call of hlop_use * rw_pattern list
           | RW_Const of (Literal.literal * ty)
           | RW_Var of var_use

    withtype lambda = (var_bind * var_pat list * var_pat list * ty list * exp)

    type code = defn list

  (* RHS of primitive value declarations *)
    datatype prim_val_rhs
      = VarPrimVal of var_use
      | HLOpPrimVal of hlop_use
      | LambdaPrimVal of lambda

  (* debugging support *)
    fun rhsToString rhs = (case rhs
	   of RHS_Mark _ => "RHS_Mark"
	    | RHS_Exp _ => "RHS_Exp"
	    | RHS_SimpleExp e => concat["RHS_SimpleExp(", simpleExpToString e, ")"]
	    | RHS_Update _ => "RHS_Update"
	    | RHS_Promote _ => "RHS_Promote"
	    | RHS_CCall _ => "RHS_CCall"
	    | RHS_VPStore _ => "RHS_VPStore"
	  (* end case *))

    and simpleExpToString e = (case e
	   of SE_Mark _ => "SE_Mark"
	    | SE_Var _ => "SE_Var"
	    | SE_Alloc _ => "SE_Alloc"
	    | SE_Wrap _ => "SE_Wrap"
	    | SE_Select _ => "SE_Select"
	    | SE_Unwrap _ => "SE_Unwrap"
	    | SE_AddrOf _ => "SE_AddrOf"
	    | SE_Const _ => "SE_Const"
	    | SE_MLString _ => "SE_MLString"
	    | SE_Cast _ => "SE_Cast"
	    | SE_Prim _ => "SE_Prim"
	    | SE_HostVProc => "SE_HostVProc"
	    | SE_VPLoad _ => "SE_VPLoad"
	    | SE_VPAddr _ => "SE_VPAddr"
	  (* end case *))

  end
