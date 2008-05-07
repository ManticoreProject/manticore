(* parse-tree.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Parse-tree representation of MinManticore programs.
 *)

structure ParseTree =
  struct

    type tyid = Atom.atom
    type tyvar = Atom.atom
    type conid = Atom.atom
    type vid = Atom.atom
    type opid = Atom.atom	(* operator IDs; e.g., "=", "<=", "<", "::", ... *)
    type modid = Atom.atom      (* module identifiers *)
    type sigid = Atom.atom      (* signature identifiers *)

  (* a term marked with a source-map span *)
    type 'a mark = {span : Error.span, tree : 'a}

  (* qualified names *)
    type 'a path = (Atom.atom list * 'a) mark

   (* signature expressions *)
    datatype sign
      = MarkSig of sign mark
      | NameSig of (sigid * ty_decl list)
      | ExpSig of spec list
    
  (* signature specifications *)
    and spec
      = MarkSpec of spec mark
      | IncludeSpec of sign
      | ModuleSpec of (modid * sign)
      | TypeSpec of ty_decl
      | ConstSpec of (conid * tyvar list)
      | ValSpec of (vid * tyvar list * ty)

  (* module expressions *)
    and module
      = MarkMod of module mark
      | DeclsMod of decl list
      | NamedMod of modid path
      | ApplyMod of (modid * module list)

  (* top-level declarations *)
    and decl
      = MarkDecl of decl mark
      | ModuleDecl of (modid * sign option * module)
      | TyDecl of ty_decl
      | ExnDecl of (conid * ty option)
      | ValueDecl of val_decl
      | LocalDecl of (decl list * decl list)
      | SignDecl of (sigid * sign)

  (* type declarations *)
    and ty_decl
      = MarkTyDecl of ty_decl mark
      | TypeTyDecl of (tyvar list * tyid * ty)
      | DataTyDecl of (tyvar list * tyid * con_decl list)

  (* data-constructor definitions *)
    and con_decl
      = MarkConDecl of con_decl mark
      | ConDecl of conid * ty option

  (* value declarations *)
    and val_decl
      = MarkVDecl of val_decl mark
      | ValVDecl of pat * exp
      | PValVDecl of pat * exp
      | FunVDecl of funct list

  (* function definitions *)
    and funct
      = MarkFunct of funct mark
      | Funct of (vid * pat * exp)

  (* types *)
    and ty
      = MarkTy of ty mark
      | NamedTy of (ty list * tyid path)
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
      | BinaryExp of (exp * opid * exp)	(* infix binary expressions *)
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
      | IdExp of vid path		(* either variable or nullary constant *)
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
      | BinaryPat of pat * conid path * pat	(* infix pattern *)
      | ConPat of conid path * pat
      | TuplePat of pat list
      | ConstPat of const
      | WildPat
      | IdPat of vid    		(* either variable or nullary constant *)
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
