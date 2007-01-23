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
    datatype func = FUNC of {
	lab : label,
	kind : func_kind,
	params : var list,
	body : exp
      }

    and func_kind
      = StandardFunc	(* a function that may be called from unknown sites; it uses the *)
			(* standard calling convention. *)
      | ContFunc	(* a continuation that may be thrown to from unknown sites; it uses *)
			(* the standard continuation-calling convention *)
      | KnownFunc	(* a function/continuation for which we know all of its call sites *)
			(* and only known functions are called from those sites (Serrano's *)
			(* "T" property).  It uses a specialized calling convention. *)
      | Block		(* a function/continuation for which we know all of its call sites *)
			(* and it is the only function called at those sites (Serrano's *)
			(* "X" property) *)

    and exp = Exp of (ProgPt.ppt * exp')

    and exp'
      = E_Let of (var list * rhs * exp)
      | E_HeapCheck of (word * exp)
      | E_If of (var * jump * jump)
      | E_Switch of (var * (int * jump) list * jump option)
      | E_Apply of (var * var list)
      | E_Throw of (var * var list)
      | E_Goto of jump

    and rhs
(* QUESTION: perhaps allow parallel copy? *)
      = E_Var of var
      | E_Label of label
      | E_Literal of Literal.literal
      | E_Select of (int * var)		(* select i'th field (zero-based) *)
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
      | Local			(* local to module *)

    withtype var = (var_kind, ty) VarRep.var_rep
	 and label = (label_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
         and jump = (label * var list)

    fun tyToString _ = ""	(* FIXME *)
    fun labelKindToString _ = ""	(* FIXME *)
    fun varKindToString _ = ""	(* FIXME *)

    structure Label = VarFn (
      struct
	type kind = label_kind
	type ty = ty
	val kindToString = labelKindToString
	val tyToString = tyToString
      end)

    datatype module = MODULE of {
	code : func list,	(* first function is initialization *)
	funcs : func Label.Map.map
      }

    structure Var = VarFn (
      struct
	type kind = var_kind
	type ty = ty
	val kindToString = varKindToString
	val tyToString = tyToString
      end)

  (* smart constructors *)
    fun mkLet (lhs, rhs, e) = let
	  val e = Exp(ProgPt.new(), E_Let(lhs, rhs, e))
	  in
	    List.app (fn x => Var.setKind(x, VK_Let rhs)) lhs;
	    e
	  end
    fun mkHeapCheck arg = Exp(ProgPt.new(), E_HeapCheck arg)
    fun mkIf arg = Exp(ProgPt.new(), E_If arg)
    fun mkSwitch arg = Exp(ProgPt.new(), E_Switch arg)
    fun mkApply arg = Exp(ProgPt.new(), E_Apply arg)
    fun mkThrow arg = Exp(ProgPt.new(), E_Throw arg)
    fun mkGoto arg = Exp(ProgPt.new(), E_Goto arg)

    fun mkModule code = MODULE{
	    code = code,
	    funcs = List.foldl
	      (fn (f as FUNC{lab, ...}, fm) => Label.Map.insert(fm, lab, f))
		Label.Map.empty code
	  }

  end
