(* ast.sml
 *
 * COPYRIGHT (c) 2006 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure AST =
  struct

  (* source file information *)
    type info = unit (* FIXME *)

    type label = Atom.atom

    type 'a field = (info * label * 'a)

    datatype sig_vis = OPAQUE | TRANSLUCENT

    datatype top_dec
      = TD_Module of (info * mod_name * module_type option * module)
      | TD_Exn of exn_def
      | TD_Dec of dec

    and module_type
      = OPAQUE of info * sign
      | TRANSLUCENT of info * sign

    and sign
      = SIG_Id of info * sig_name
      | SIG_Body of info * spec list

    and spec
      = S_Module of (info * mod_name * module_type)
      | S_Exn of exn_name
      | S_Val of var

    and module
      = M_Id of info * mod_name
      | M_Body of info * top_dec list

    and exn_def
      = EX_Alias of (info * exn_name * exn_name)
      | EX_Exn of (info * exn_name)

    and exp
      = E_Lit of (info * Literal.literal)
      | E_Var of (info * var)
      | E_Con of (info * datacon)
      | E_Record of (info * exp field list)
      | E_Let of (info * dec list * exp)
      | E_App of (info * exp * exp)
      | E_TyApp of (info * exp * ty list)
      | E_Handle of (info * exp * match)
      | E_Case of (info * exp * match)
      | E_Fn of (info * match)
    (* parallel forms *)
      | E_PArray of (info * exp list)
      | E_PComp of (info * exp * pbind list * exp option)
      | E_Spawn of (info * exp)

    and dec
      = D_Val of (info * pat * exp)
      | D_Rec of (info * (var * exp) list)

    and pbind
      = PBind of (info * pat * exp)

    and match
      = Case of info * rule list

    and rule
      = Rule of (info * pat * exp option * exp)

    and pat
      = P_Wild of info
      | P_Lit of info * literal
      | P_Var of info * var
      | P_Con of info * datacon * ty list * pat list
      | P_Record of info * pat field list
      | P_As of info * var * pat

    and ty
      = T_Var of info * ty_var
      | T_Record of (info * ty field list)
      | T_TyCon of (info * ty list * tycon)
      | T_Fun of (info * ty * ty)

    withtype var = (var_binding, ty) VarRep.var_rep

  end
