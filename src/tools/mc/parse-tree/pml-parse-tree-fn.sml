(* pml-parse-tree-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of Manticore programs.
 *)

functor PMLParseTreeFn (

    type ty_bind	(* top-level type identifiers *)
    type ty_use		(* type-variable uses *)

    type con_bind	(* data constructor binds *)
    type con_use	(* data constructor uses *)

    type var_bind	(* variable binds *)
    type var_use	(* variable uses *)

    type op_id		(* operator IDs; e.g., "=", "<=", "<", "::", ... *)

    type mod_bind	(* module bindings *)
    type mod_use	(* module uses *)

    type sig_id		(* signature identifiers *)

  ) = struct

  (* a term marked with a source-map span *)
    type 'a mark = 'a Error.mark

    type tyvar = Atom.atom          (* type-variable identifiers *)
    type ty_bind = ty_bind
    type ty_use = ty_use
    type con_bind = con_bind
    type con_use = con_use
    type var_bind = var_bind
    type var_use = var_use
    type op_id = op_id
    type mod_bind = mod_bind
    type mod_use = mod_use
    type sig_id = sig_id

    structure BOMParseTree = BOMParseTreeFn (
	type var_use = var_use
	type var_bind = var_bind
	type pml_var = var_use
	type ty_con = ty_use
	type ty_def = ty_bind
	type prim = var_use
	type dcon = con_use
	type hlop_bind = var_bind
	type hlop_use = var_use
	type c_id = var_bind      (* treat C functions as regular identifiers *)
      )

   (* signature expressions *)
    datatype sign
      = MarkSig of sign mark
      | NameSig of (sig_id * ty_decl list)
      | ExpSig of spec list
    
  (* signature specifications *)
    and spec
      = MarkSpec of spec mark
      | IncludeSpec of sign
      | ModuleSpec of (mod_bind * sign)
      | TypeSpec of ty_decl
      | ConstSpec of (con_bind * tyvar list)
      | ValSpec of (var_bind * tyvar list * ty)

  (* module expressions *)
    and module
      = MarkMod of module mark
      | DeclsMod of decl list
      | NamedMod of mod_use
      | ApplyMod of (mod_use * module list)

  (* top-level declarations *)
    and decl
      = MarkDecl of decl mark
      | ModuleDecl of (mod_bind * sign option * module)
      | TyDecl of ty_decl
      | ExnDecl of (con_bind * ty option)
      | ValueDecl of val_decl
      | LocalDecl of (decl list * decl list)
      | SignDecl of (sig_id * sign)
      | PrimCodeDecl of BOMParseTree.code

  (* type declarations *)
    and ty_decl
      = MarkTyDecl of ty_decl mark
      | TypeTyDecl of (tyvar list * ty_bind * ty)
      | DataTyDecl of (tyvar list * ty_bind * con_decl list)
      | AbsTyDecl of (tyvar list * ty_bind)
      | PrimTyDecl of (tyvar list * ty_bind * BOMParseTree.ty)

  (* data-constructor definitions *)
    and con_decl
      = MarkConDecl of con_decl mark
      | ConDecl of con_bind * ty option

  (* value declarations *)
    and val_decl
      = MarkVDecl of val_decl mark
      | ValVDecl of pat * exp
      | PValVDecl of pat * exp
      | FunVDecl of funct list
      | PrimVDecl of pat * BOMParseTree.prim_val_rhs

  (* function definitions *)
    and funct
      = MarkFunct of funct mark
      | Funct of (var_bind * pat * exp)

  (* types *)
    and ty
      = MarkTy of ty mark
      | NamedTy of (ty list * ty_use)
      | VarTy of tyvar
      | TupleTy of ty list
      | FunTy of (ty * ty)

  (* expressions *)
    and exp
      = MarkExp of exp mark
      | LetExp of (val_decl list * exp)
      | IfExp of (exp * exp * exp)
      | CaseExp of (exp * match list)
      | PCaseExp of (exp list * pmatch list)
      | HandleExp of (exp * match list)
      | RaiseExp of exp
      | AndAlsoExp of (exp * exp)
      | OrElseExp of (exp * exp)
      | BinaryExp of (exp * op_id * exp)	(* infix binary expressions *)
      | PChoiceExp of exp list		(* two or more expressions joined by |?| *)
      | ApplyExp of (exp * exp)		(* application *)
      | ConstExp of const
      | TupleExp of exp list
      | ListExp of exp list
      | RangeExp of (exp * exp * exp option)
      | PTupleExp of exp list
      | PArrayExp of exp list
      | PCompExp of (exp * pbind list * exp option)
      | SpawnExp of exp
      | SeqExp of exp list		(* sequence of two or more expressions *)
      | IdExp of var_use		(* either variable or nullary constant *)
      | ConstraintExp of exp * ty	(* type constraint *)

  (* pattern matching rules *)
    and match
      = MarkMatch of match mark
      | Match of (pat * exp)

    and pmatch (* parallel matches, for use in pcase *)
      = MarkPMatch of pmatch mark
      | PMatch of (ppat list * exp)
      | Otherwise of exp

    and pbind
      = MarkPBind of pbind mark
      | PBind of (pat * exp)

    and pat
      = MarkPat of pat mark
      | BinaryPat of pat * con_use * pat	(* infix pattern *)
      | ConPat of con_use * pat
      | TuplePat of pat list
      | ConstPat of const
      | WildPat
      | IdPat of var_bind    		(* either variable or nullary constant *)
      | ConstraintPat of pat * ty	(* type constraint *)

    and ppat (* parallel patterns, for use in pcase *)
      = MarkPPat of ppat mark
      | NDWildPat (* non-deterministic wildcard *)
      | HandlePat of pat
      | Pat of pat

  (* literal values *)
    and const
      = IntLit of IntInf.int
      | FltLit of FloatLit.float
      | StrLit of string

    type program = decl list mark

  end
