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
      | IfExp of (exp * exp * exp * ty)			(* ty is result type *)
      | CaseExp of (exp * match list * ty)		(* ty is result type *)
      | HandleExp of (exp * match list * ty)		(* ty is result type *)
      | RaiseExp of (exp * ty)				(* ty is result type *)
      | FunExp of (var * exp * ty)			(* ty is result type *)
      | ApplyExp of exp * exp * ty			(* ty is result type *)
      | VarArityOpExp of var_arity_op * int * ty        (* ty is operator type *)                 
      | TupleExp of exp list
      | RangeExp of (exp * exp * exp option * ty)	(* ty is element type *)
      | PTupleExp of exp list
      | PArrayExp of exp list * ty			(* ty is element type *)
      | PCompExp of (exp * (pat * exp) list * exp option)
      | PChoiceExp of exp list * ty			(* ty is result type *)
      | SpawnExp of exp
      | ConstExp of const
      | VarExp of var * ty list
      | SeqExp of (exp * exp)
      | OverloadExp of overload_var ref

    and binding
      = ValBind of pat * exp
      | PValBind of pat * exp
      | DValBind of pat * exp	(* temporary *)
      | FunBind of lambda list

    and lambda = FB of (var * var * exp)

    and var_arity_op 
      = MapP

    and match
      = PatMatch of pat * exp
      | CondMatch of pat * exp * exp		(* conditional match; not used yet *)

    and pat
      = ConPat of dcon * ty list * pat	(* data-constructor application *)
      | TuplePat of pat list
      | VarPat of var
      | WildPat of ty
      | ConstPat of const

    and const
      = DConst of dcon * ty list
      | LConst of (Literal.literal * ty)

    and overload_var
      = Unknown of (ty * var list)
      | Instance of var

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

    datatype module = Module of {
	exns : dcon list,
	body : exp
      }

  end
