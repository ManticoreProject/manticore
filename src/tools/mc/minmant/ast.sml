(* ast.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure AST =
  struct

    datatype ty_scheme = datatype Types.ty_scheme
    datatype ty = datatype Types.ty
    datatype tyvar = datatype Types.tyvar
    datatype dcon = datatype Types.dcon

    datatype exp
      = LetExp of binding * exp
      | IfExp of (exp * exp * exp)
      | CaseExp of (exp * (pat * exp) list)
      | ApplyExp of exp * exp
      | TupleExp of exp list
      | RangeExp of (exp * exp * exp option)
      | PTupleExp of exp list
      | PArrayExp of exp list
      | ComprehendExp of (exp * (pat * exp) list * exp option)
      | SpawnExp of exp
      | ConstExp of const
      | VarExp of var * ty list
      | SeqExp of (exp * exp)

    and binding
      = ValBind of pat * exp
      | FunBind of lambda list

    and lambda = FB of (var * var * exp)

    and pat
      = ConPat of dcon * ty list * pat list	(* data-constructor application *)
      | TuplePat of pat list
      | VarPat of var
      | ConstPat of const

    and const
      = DConst of dcon * ty list
      | LConst of (Literal.literal * ty)

    and var_kind
      = VK_None
      | VK_Pat			(* bound in a pattern *)
      | VK_Fun			(* bound to a function *)
      | VK_Prim			(* builtin function or operator *)

    withtype var = (var_kind, ty_scheme ref) VarRep.var_rep

    fun varKindToString VK_None = "None"
      | varKindToString VK_Pat = "Pat"
      | varKindToString VK_Fun = "Fun"
      | varKindToString VK_Prim = "Prim"

    type module = exp

  end
