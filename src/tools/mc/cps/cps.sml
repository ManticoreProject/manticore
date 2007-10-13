(* cps.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPS =
  struct

    type ty = CPSTy.ty

    type tag = Word.word	(* data-constant tags *)
    type offset = IntInf.int	(* offsets into the runtime-system vproc structure *)

    datatype exp
      = Let of (var list * rhs * exp)
      | Fun of (lambda list * exp)
      | Cont of (lambda * exp)
      | If of (var * exp * exp)
      | Switch of (var * (tag * exp) list * exp option)
      | Apply of (var * var list * var list)
      | Throw of (var * var list)

    and rhs
      = Var of var list
      | Cast of ty * var		(* typecast *)
      | Const of (Literal.literal * ty)
      | Select of (int * var)		(* select i'th field (zero-based) *)
      | Update of (int * var * var)	(* update i'th field (zero-based) *)
      | AddrOf of (int * var)		(* return address of i'th field (zero-based) *)
      | Alloc of var list		(* local-heap allocation *)
      | GAlloc of var list		(* global-heap allocation *)
      | Promote of var			(* promote a heap object to the global heap *)
      | Prim of prim
      | CCall of (var * var list)
    (* VProc operations *)
      | HostVProc			(* gets the hosting VProc *)
      | VPLoad of (offset * var)	(* load a value from the given byte offset *)
					(* in the vproc structure *)
      | VPStore of (offset * var * var)	(* store a value at the given byte offset *)
					(* in the vproc structure *)

    and lambda = FB of {	      (* function/continuation abstraction *)
	  f : var,			(* function name *)
	  params : var list,		(* parameters *)
	  rets : var list,		(* return/exception continuations *)
	  body : exp			(* function body *)
	}

    and var_kind
      = VK_None
      | VK_Let of rhs
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_Param of lambda
      | VK_Extern of string

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim

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
	      val tyToString = CPSTyUtil.toString
	    end)
	in
	open V
	fun isExtern (VarRep.V{kind = ref(VK_Extern _), ...}) = true
	  | isExtern _ = false
	end (* local *)
      end

  (* representation of true and false *)
    val trueRHS = Const(Literal.trueLit, CPSTy.boolTy)
    val falseRHS = Const(Literal.falseLit, CPSTy.boolTy)

  (* representation of unit *)
    val unitRHS = Const(Literal.unitLit, CPSTy.unitTy)

  (* representation of nil *)
    val nilRHS = Const(Literal.nilLit, CPSTy.T_Enum 0w0)

  (* smart constructors; these enforce the variable kind invariant and should be
   * used to construct terms.
   *)
    fun mkLet (lhs, rhs, exp) = (
	  List.app (fn x => Var.setKind(x, VK_Let rhs)) lhs;
	  Let(lhs, rhs, exp))
    fun mkFun (fbs, e) = let
	  fun setKind (lambda as FB{f, params, rets, ...}) = (
		Var.setKind(f, VK_Fun lambda);
		List.app (fn x => Var.setKind(x, VK_Param lambda)) params;
		List.app (fn x => Var.setKind(x, VK_Param lambda)) rets)
	  in
	    List.app setKind fbs;
	    Fun(fbs, e)
	  end
    fun mkCont (lambda as FB{f, params, ...}, e) = (
	  Var.setKind(f, VK_Cont lambda);
	  List.app (fn x => Var.setKind(x, VK_Param lambda)) params;
	  Cont(lambda, e))

    fun mkCFun arg = (
	  Var.setKind(#var arg, VK_Extern(#name arg));
	  CFunctions.CFun arg)

  end
