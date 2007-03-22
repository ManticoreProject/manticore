(* cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPS =
  struct

    type ty = CPSTy.ty

    type offset = IntInf.int

    datatype exp
      = Let of (var list * rhs * exp)
      | Fun of (lambda list * exp)
      | Cont of (lambda * exp)
      | If of (var * exp * exp)
      | Switch of (var * (int * exp) list * exp option)
      | Apply of (var * var list)
      | Throw of (var * var list)
    (* scheduler operations *)
      | Run of {vp : var, act : var, fiber : var}
      | Forward of {vp : var, sign : var}

    and rhs
      = Var of var list
      | Enum of Word.word
      | Cast of ty * var		(* typecast *)
      | Literal of Literal.literal
      | Select of (int * var)		(* select i'th field (zero-based) *)
      | Alloc of var list
      | Wrap of var			(* wrap raw value *)
      | Unwrap of var			(* unwrap value *)
      | Prim of prim
      | CCall of (var * var list)
    (* VProc operations *)
      | HostVProc			(* gets the hosting VProc *)
      | VPLoad of (offset * var)	(* load a value from the given byte offset *)
					(* in the vproc structure *)
      | VPStore of (offset * var * var)	(* store a value at the given byte offset *)
					(* in the vproc structure *)

    and var_kind
      = VK_None
      | VK_Let of rhs
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_Param of lambda
      | VK_Extern of string

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and lambda = (var * var list * exp)

    datatype module = MODULE of {
	name : Atom.atom,
	externs : var CFunctions.c_fun list,
	body : lambda
      }

    fun varKindToString VK_None = "None"
      | varKindToString (VK_Let _) = "Let"
      | varKindToString (VK_Fun _) = "Fun"
      | varKindToString (VK_Cont _) = "Cont"
      | varKindToString (VK_Param _) = "Param"
      | varKindToString (VK_Extern _) = "Extern"

    structure Var = struct
	local
	  structure V = VarFn (
	    struct
	      type kind = var_kind
	      type ty = ty
	      val defaultKind = VK_None
	      val kindToString = varKindToString
	      val tyToString = CPSTy.toString
	    end)
	in
	open V
	fun isExtern (VarRep.V{kind = ref(VK_Extern _), ...}) = true
	  | isExtern _ = false
	end (* local *)
      end

  (* representation of true and false *)
    val trueLit = Enum 0w1
    val falseLit = Enum 0w0

  (* representation of unit *)
    val unitLit = Enum 0w0

  (* representation of nil *)
    val nilLit = Enum 0w0

  (* smart constructors; these enforce the variable kind invariant and should be
   * used to construct terms.
   *)
    fun mkLet (lhs, rhs, exp) = (
	  List.app (fn x => Var.setKind(x, VK_Let rhs)) lhs;
	  Let(lhs, rhs, exp))
    fun mkFun (fbs, e) = let
	  fun setKind (lambda as (f, params, _)) = (
		Var.setKind(f, VK_Fun lambda);
		List.app (fn x => Var.setKind(x, VK_Param lambda)) params)
	  in
	    List.app setKind fbs;
	    Fun(fbs, e)
	  end
    fun mkCont (lambda as (k, params, _), e) = (
	  Var.setKind(k, VK_Cont lambda);
	  List.app (fn x => Var.setKind(x, VK_Param lambda)) params;
	  Cont(lambda, e))

    fun mkCFun arg = (
	  Var.setKind(#var arg, VK_Extern(#name arg));
	  CFunctions.CFun arg)

  end
