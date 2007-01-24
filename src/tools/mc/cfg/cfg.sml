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

    datatype ty = datatype CFGTy.ty

  (* extended basic block *)
    datatype func = FUNC of {
	lab : label,		(* label of function *)
	entry : convention,	(* calling convention, includes parameters *)
	body : exp list,	(* body of function is straight-line sequence of bindings *)
	exit : transfer		(* control transfer out of function *)
      }

    and convention
      = StdFunc of {	(* a function that may be called from unknown sites; it uses the *)
			(* standard calling convention. *)
	    clos : var,	  (* closure parameter *)
	    arg : var,	  (* argument parameter *)
	    ret : var,	  (* return-continuation parameter *)
	    exh : var	  (* exception-handler-continuation parameter *)
	  }
      | StdCont of {	(* a continuation that may be thrown to from unknown sites; it uses *)
			(* the standard continuation-calling convention *)
	    clos : var,	  (* closure parameter *)
	    arg : var	  (* argument parameter *)
	  }
      | KnownFunc	(* a function/continuation for which we know all of its call sites *)
			(* and only known functions are called from those sites (Serrano's *)
			(* "T" property).  It uses a specialized calling convention. *)
	  of var list	  (* parameters *)
      | Block		(* a function/continuation for which we know all of its call sites *)
			(* and it is the only function called at those sites (Serrano's *)
			(* "X" property) *)
	  of var list	  (* parameters *)

    and exp
      = E_Var of var list * var list
      | E_Label of var * label
      | E_Literal of var * Literal.literal
      | E_Select of (var * int * var)		(* select i'th field (zero-based) *)
      | E_Alloc of var * var list
      | E_Prim of var * prim
      | E_CCall of (var * var * var list)

    and transfer
      = StdApply of {f : var, clos : var, arg : var, ret : var, exh : var}
      | StdThrow of {k : var, clos : var, arg : var}
      | Apply of {f : var, args : var list}
      | Goto of jump
      | If of (var * jump * jump)
      | Switch of (var * (int * jump) list * jump option)
      | HeapCheck of {szb : word, gc : jump, nogc : jump}

    and var_kind
      = VK_None
      | VK_Let of exp
      | VK_Param of func

    and label_kind
      = Extern of string	(* external label; e.g., a C function *)
      | Export of string	(* exported label *)
      | Local			(* local to module *)

    withtype var = (var_kind, ty) VarRep.var_rep
	 and label = (label_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
         and jump = (label * var list)

    fun labelKindToString (Extern s) = "Extern " ^ s
      | labelKindToString (Export s) = "Export " ^ s
      | labelKindToString Local = "Local"

    fun varKindToString VK_None = "None"
      | varKindToString (VK_Let _) = "Let"
      | varKindToString (VK_Param _) = "Param"

    structure Label = VarFn (
      struct
	type kind = label_kind
	type ty = ty
	val kindToString = labelKindToString
	val tyToString = CFGTy.toString
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
	val tyToString = CFGTy.toString
      end)

  (* project out the lhs variables of an expression *)
    fun lhsOfExp (E_Var(xs, _)) = xs
      | lhsOfExp (E_Label(x, _)) = [x]
      | lhsOfExp (E_Literal(x, _)) = [x]
      | lhsOfExp (E_Select(x, _, _)) = [x]
      | lhsOfExp (E_Alloc(x, _)) = [x]
      | lhsOfExp (E_Prim(x, _)) = [x]
      | lhsOfExp (E_CCall(x, _, _)) = [x]

  (* project out the rhs variable of an expression *)
    fun rhsOfExp (E_Var(_, ys)) = ys
      | rhsOfExp (E_Label _) = []
      | rhsOfExp (E_Literal _) = []
      | rhsOfExp (E_Select(_, _, y)) = [y]
      | rhsOfExp (E_Alloc(_, args)) = args
      | rhsOfExp (E_Prim(_, p)) = Prim.varsOf p
      | rhsOfExp (E_CCall(_, f, args)) = f::args

  (* project the list of variables in a control transfer *)
    fun varsOfXfer (StdApply{f, clos, arg, ret, exh}) = [f, clos, arg, ret, exh]
      | varsOfXfer (StdThrow{k, clos, arg}) = [k, clos, arg]
      | varsOfXfer (Apply{f, args}) = f::args
      | varsOfXfer (Goto(_, args)) = args
      | varsOfXfer (If(x, (_, args1), (_, args2))) = x :: args1 @ args2
      | varsOfXfer (Switch(x, cases, dflt)) = let
	  fun f ((_, (_, args)), l) = args @ l
	  in
	    x :: (List.foldl f (case dflt of SOME(_, args) => args | _ => []) cases)
	  end
      | varsOfXfer (HeapCheck{gc=(_, args1), nogc=(_, args2), ...}) = args1 @ args2

  (* smart constructors that set the kind field of the lhs variables *)
    fun mkExp e = (
	  List.app (fn x => Var.setKind(x, VK_Let e)) (lhsOfExp e);
	  e)
    fun mkVar arg = mkExp(E_Var arg)
    fun mkLabel arg = mkExp(E_Label arg)
    fun mkLiteral arg = mkExp(E_Literal arg)
    fun mkSelect arg = mkExp(E_Select arg)
    fun mkAlloc arg = mkExp(E_Alloc arg)
    fun mkPrim arg = mkExp(E_Prim arg)
    fun mkCCall arg = mkExp(E_CCall arg)

    fun mkFunc (l, conv, body, exit) = let
	  val func = FUNC{lab = l, entry = conv, body = body, exit = exit}
	  in
(* FIXME: set kind of variables *)
	    func
	  end

    fun mkModule code = MODULE{
	    code = code,
	    funcs = List.foldl
	      (fn (f as FUNC{lab, ...}, fm) => Label.Map.insert(fm, lab, f))
		Label.Map.empty code
	  }

  end
