(* cfg.sml
 *
 * COPYRIGHT (c) 2006 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure CFG =
  struct

    type ty = unit (* FIXME *)

  (* extended basic block *)
    datatype xblock = XBB of (label * var list * exp)

    and exp = Exp of (ProgPt.prog_pt * exp')

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

    and var_kind
      = VK_None
      | VK_Let of rhs
      | VK_Param

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
         and jump = (label * var list)

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
