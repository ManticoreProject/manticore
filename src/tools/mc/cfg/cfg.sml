(* cfg.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The "control-flow graph" representation; essentially a 1st-order
 * CPS language.
 *)

structure CFG = struct

  datatype ty = WordArray of int
			   
  and exp = Exp of ProgPt.ppt * exp'

  and exp' = E_Let of (var list * rhs * exp)
	   | E_HeapCheck of (int * jump * exp)
	   | E_If of (var * jump * jump)
	   | E_Switch of (var * (int * jump) list * jump option)
	   | E_Apply of (var * var list)
	   | E_Throw of (var * var list)
	   | E_Goto of jump

  and rhs = E_Var of var
	  | E_Label of label
	  | E_Literal of Literal.literal
	  | E_Select of (int * var)
	  | E_Alloc of (ty * var list)
	  | E_Prim of prim
	  | E_CCall of (var * var list)

  and var_kind = VK_None
	       | VK_Let of rhs
	       | VK_Param

  and label_kind = Extern of string
		 | Export of string
		 | Local

  withtype var = (var_kind, ty) VarRep.var_rep
  and label    = (label_kind, ty) VarRep.var_rep
  and prim     = var Prim.prim
  and jump     = (label * var list)

  datatype func_kind
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

  datatype func = FUNC of {
	   lab : label,
	   kind : func_kind,
	   params : var list,
	   body : exp
  }

  fun tyToString (WordArray i) = "WordArray " ^ Int.toString i
  fun labelKindToString (Extern s) = "Extern " ^ s
    | labelKindToString (Export s) = "Export " ^ s
    | labelKindToString Local = "Local"

  structure Label = VarFn (
		    struct
		      type kind = label_kind
		      type ty = ty
		      val kindToString = labelKindToString
		      val tyToString = tyToString
		    end)

  datatype module = MODULE of {
	   code : func list,  (* the first function is initialization *)
	   funcs : func Label.Map.map
  }

  fun kindToString VK_None = "None"
    | kindToString (VK_Let rhs) = "Let"
    | kindToString VK_Param = "Param"

  structure Var = VarFn (
		  struct
		    type kind = var_kind
		    type ty = ty
		    val kindToString = kindToString
		    val tyToString = tyToString
		  end)

  (* smart constructors *)
  fun mkExp e = Exp (ProgPt.new (), e)
  fun mkLet (lhs, rhs, e) = 
      let fun setKind v = Var.setKind (v, VK_Let rhs)
      in
	  app setKind lhs;
	  mkExp (E_Let (lhs, rhs, e))
      end
  fun mkHeapCheck arg = mkExp (E_HeapCheck arg)
  fun mkIf arg = mkExp (E_If arg)
  fun mkSwitch arg = mkExp (E_Switch arg)
  fun mkApply arg = mkExp (E_Apply arg)
  fun mkThrow arg = mkExp (E_Throw arg)
  fun mkGoto arg = mkExp (E_Goto arg)
  fun mkModule code = 
      let fun insFunc (f as FUNC {lab, ...}, funcs) = Label.Map.insert (funcs, lab, f)
	  val funcs = foldl insFunc Label.Map.empty code 
      in
	  MODULE {code=code, funcs=funcs}
      end

end (* CFG *)
