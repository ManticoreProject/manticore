(* east.sml
 *
 * ASTs with effects (E+AST).
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure EAST =
  struct

    datatype ty_scheme = datatype TETypes.ty_scheme
    datatype ty = datatype TETypes.ty
    datatype tyvar = datatype Types.tyvar
    datatype dcon = datatype Types.dcon

    type info = Error.span   (* source file information *)

    type label = Atom.atom

    type 'a field = (info * label * 'a)

    type sig_name = Stamp.stamp

    datatype module_exp
      = MEXP_BODY of top_dec list
      | MEXP_NAME of module_ref

    and top_dec
      = TD_Module of (info * module_ref * module_type option * module)
      | TD_DCon of dcon
      | TD_Binding of binding

    and module_ref = MOD of {                        (* reference to a module *)
          name : Atom.atom,
	  id : Stamp.stamp,                          (* unique id *)
	  formals : module_ref list option,          (* formal parameters to a functor *)
	  expansionOpts : ExpansionOpts.opt list ref (* compiler options for expanding expressions *)
        }
 
    and module_type
      = OPAQUE of info * sign

    and sign
      = SIG_Id of info * sig_name
      | SIG_Body of info * spec list

    and spec
      = S_Module of (info * module_ref * module_type)
      | S_Dcon of dcon
      | S_Val of var

    and module
      = M_Id of info * module_ref
      | M_Body of info * top_dec list

    and exp
      = LetExp of (binding * exp)
      | IfExp of (exp * exp * exp * ty)			(* ty is result type *)
      | CaseExp of (exp * match list * ty)		(* ty is result type *)
      | PCaseExp of (exp list * pmatch list * ty)       (* ty is result type *)
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
      | PChoiceExp of (exp list * ty)			(* ty is result type *)
      | SpawnExp of exp
      | ConstExp of const
      | VarExp of (var * ty list)
      | SeqExp of (exp * exp)
      | OverloadExp of overload_var ref
      | ExpansionOptsExp of (ExpansionOpts.opt list * exp)         (* compiler options for expanding expressions *)

    and binding
      = ValBind of pat * exp
      | PValBind of pat * exp
      | FunBind of lambda list
      | PrimVBind of var * ProgramParseTree.PML2.BOMParseTree.prim_val_rhs
      | PrimCodeBind of ProgramParseTree.PML2.BOMParseTree.code              (* BOM definitions *)

    and lambda = FB of (var * var * exp)

    and var_arity_op 
      = MapP

    and match
      = PatMatch of pat * exp
      | CondMatch of pat * exp * exp		(* conditional match; not used yet *)

    and pmatch
      = PMatch of ppat list * exp
      | Otherwise of ty list * exp

    and pat
      = ConPat of dcon * ty list * pat	(* data-constructor application *)
      | TuplePat of pat list
      | VarPat of var
      | WildPat of ty
      | ConstPat of const

    and ppat
      = NDWildPat of ty
      | HandlePat of pat * ty (* handle pats, like wild pats, need to be typed *)
      | Pat of pat 

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

    type comp_unit = top_dec list

    fun varKindToString VK_None = "None"
      | varKindToString VK_Pat = "Pat"
      | varKindToString VK_Fun = "Fun"
      | varKindToString VK_Prim = "Prim"

  end
