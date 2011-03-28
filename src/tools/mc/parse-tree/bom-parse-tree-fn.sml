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
    val ty_defToString    : ty_def -> string
    val ty_conToString    : ty_con -> string
    val var_useToString   : var_use -> string
    val var_bindToString  : var_bind -> string
    val hlop_bindToString : hlop_bind -> string
    val hlop_useToString  : hlop_use -> string
    val c_idToString      : c_id -> string
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
      | D_Define of (opt_attr list * hlop_bind * var_pat list * var_pat list * ty list option * exp option)
		                                   (* HLOp *)
      | D_ImportML of (opt_attr list * hlop_bind * pml_var) (* form to import an ML function *)
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

    and opt_attr
      = A_Constr
      | A_Inline
      | A_Pure

  (* rewrite pattern *)
    and rw_pattern = 
	RW_HLOpApply of (hlop_use * rw_pattern list)     (* application of a hlop *)
      | RW_Prim of (prim * rw_pattern list)              (* application of a prim-op or data constructor  *)
      | RW_Const of (Literal.literal * ty)
      | RW_Var of var_bind
      | RW_Alloc of rw_pattern list                      (* allocation in the local heap *)

    withtype lambda = (var_bind * var_pat list * var_pat list * ty list * exp)

    type code = defn list

  (* RHS of primitive value declarations *)
    datatype prim_val_rhs
      = VarPrimVal of var_use
      | HLOpPrimVal of hlop_use
      | LambdaPrimVal of lambda

  (* string makers *)

    fun tyToString t = let
      fun par s t = s ^ "(" ^ t ^ ")"
      val catw = String.concatWith
      val cm = catw ","
      val sp = catw " "
      fun lp (T_Mark {tree, ...}) = lp tree
	| lp T_Any = "ANY"
	| lp (T_Enum w) = par "enum" (Word.toString w)
	| lp (T_Raw r) = par "raw" (RawTypes.toString r)
	| lp (T_Tuple (b, ts)) = par "" (if b then "!" else "" ^ (cm o map lp) ts)
	| lp (T_Addr t) = par "addr" (lp t)
	| lp (T_Fun (ts1, ts2, ts3)) = 
            par "fun" (catw ";" (map (sp o map lp) [ts1, ts2, ts3]))
        | lp (T_Cont ts) = par "cont" (sp (map lp ts))
	| lp (T_CFun proto) = par "cfun" (CFunctions.protoToString proto)
	| lp (T_VProc) = "vproc"
	| lp (T_TyCon c) = ty_conToString c
      in
	lp t
      end

    fun var_patToString p = let
      fun s (P_VPMark {tree=p', ...}) = s p'
	| s (P_Wild optTy) = "_"
	| s (P_Var (x, t)) = var_bindToString x
      in
        s p 
      end

    (* withtype lambda = (var_bind * var_pat list * var_pat list * ty list * exp) *)

    fun prim_val_rhsToString v = (case v
      of VarPrimVal x => var_useToString x
       | HLOpPrimVal h => hlop_useToString h
       | LambdaPrimVal (f, xs, ys, ts, e) => let
	   val catw = String.concatWith
           val fS = var_bindToString f
	   val xsS = catw "," (map var_patToString xs)
	   val ysS = catw "," (map var_patToString ys)
	   val tsS = catw "," (map tyToString ts)
           in
	     concat ["Lambda(", fS, ",", xsS, ",", ysS, ",", tsS, ",_)"]
	   end
      (* end case *))

    fun defnToString d = (case d
      of D_Mark {tree=d', ...} => defnToString d'
       | D_Extern f => "D_Extern(" ^ CFunctions.nameOf f ^ "...)"
       | D_TypeDef (td, t) => "D_TypeDef(" ^ ty_defToString td ^ "...)"
       | D_Define (attrs, h, ps1, ps2, optTys, optE) => "D_Define(" ^ hlop_bindToString h ^ "...)"
       | D_ImportML (attrs, h, x) => "D_ImportML(" ^ hlop_bindToString h ^ "...)"
       | D_Rewrite {label, lhs, rhs, weight} => "D_Rewrite(...)"
      (* end case *))

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

    fun codeToString ds = String.concatWith "\n" ("" :: List.map defnToString ds)

  end
