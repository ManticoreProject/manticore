(* cfg.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The "control-flow graph" representation; essentially a 1st-order
 * CPS language.
 *)

structure CFG =
  struct

    type ty = unit (* FIXME *)

  (* extended basic block *)
    datatype xblock = XBB of (label * var list * exp)

    and exp = Exp of (ProgPt.ppt * exp')

    and exp'
      = E_Let of (var list * rhs * exp)
      | E_HeapCheck of (int * jump * exp)
      | E_If of (var * jump * jump)
      | E_Apply of (var * var list)
      | E_Throw of (var * var list)
      | E_Goto of jump

    and rhs
      = E_Var of var
      | E_Label of label
      | E_Select of (int * var)
      | E_Alloc of ty * var list
      | E_Prim of prim
      | E_CCall of (var * var list)

    and var_kind
      = VK_None
      | VK_Let of rhs
      | VK_Param

    and label_kind
      = Extern of string	(* external label; e.g., a C function *)
      | Export of string	(* exported label *)
      | Local			(* local to a function *)
      | Entry

    withtype var = (var_kind, ty) VarRep.var_rep
	 and label = (label_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
         and jump = (label * var list)

    datatype func_kind
      = KnownFunc		(* known function; use specialized calling convention *)
      | StandardFunc		(* standard calling convention *)
      | ContFunc		(* first-class continuation *)

    datatype func = FUNC of {
	entry : xblock,
	kind : func_kind,
	body : xblock list
      }

    fun tyToString _ = ""	(* FIXME *)
    fun kindToString _ = ""	(* FIXME *)

    structure Var = VarFn (
      struct
	type kind = var_kind
	type ty = ty
	val kindToString = kindToString
	val tyToString = tyToString
      end)

  end
