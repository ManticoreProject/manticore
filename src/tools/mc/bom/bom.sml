(* bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOM =
  struct

    datatype data_con = datatype BOMTyCon.data_con
    datatype dcon_rep = datatype BOMTyCon.dcon_rep

    type ty = BOMTy.ty
    type hlop = HLOp.hlop

    type offset = IntInf.int

    datatype exp = E_Pt of (ProgPt.ppt * term)

    and term
      = E_Let of (var list * exp * exp)
      | E_Stmt of (var list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (var * exp * exp)
      | E_Case of (var * (pat * exp) list * exp option)
      | E_Apply of (var * var list * var list)
      | E_Throw of (var * var list)
      | E_Ret of var list
      | E_HLOp of (hlop * var list * var list)	(* application of high-level operator *)

    and rhs
      = E_Const of const
      | E_Cast of (ty * var)
      | E_Select of (int * var)
      | E_Update of (int * var * var)		(* update i'th field (zero-based) *)
      | E_Alloc of (ty * var list)
      | E_Wrap of var
      | E_Unwrap of var
      | E_Prim of prim
      | E_DCon of (data_con * var list)		(* data constructor *)
      | E_CCall of (var * var list)		(* foreign-function calls *)
    (* VProc operations *)
      | E_HostVProc				(* gets the hosting VProc *)
      | E_VPLoad of (offset * var)		(* load a value from the given byte offset *)
						(* in the vproc structure *)
      | E_VPStore of (offset * var * var)	(* store a value at the given byte offset *)
						(* in the vproc structure *)

    and lambda = FB of {	  	    (* function/continuation abstraction *)
	  f : var,				(* function name *)
	  params : var list,			(* parameters *)
	  exh : var list,			(* exception continuation *)
	  body : exp				(* function body *)
	}

    and pat			  	    (* simple, one-level, patterns *)
      = P_DCon of data_con * var list
      | P_Const of const

    and var_kind
      = VK_None
      | VK_Let of exp
      | VK_RHS of rhs
      | VK_Param
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_Extern of string

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and const = (Literal.literal * ty)

    datatype module = MODULE of {
	name : Atom.atom,
	externs : var CFunctions.c_fun list,
	body : lambda
      }

    fun varKindToString VK_None = "None"
      | varKindToString (VK_Let _) = "Let"
      | varKindToString (VK_RHS _) = "RHS"
      | varKindToString VK_Param = "Param"
      | varKindToString (VK_Fun _) = "Fun"
      | varKindToString (VK_Cont _) = "Cont"
      | varKindToString (VK_Extern _) = "Extern"

    structure Var = struct
    	local
	  structure V = VarFn (
	    struct
	      type kind = var_kind
	      type ty=ty
	      val defaultKind = VK_None
	      val kindToString = varKindToString
	      val tyToString = BOMTy.toString
	    end)
	in
	open V
	end
      end 
       
    val trueConst = (Literal.trueLit, BOMTy.boolTy)
    val falseConst = (Literal.falseLit, BOMTy.boolTy)
    val unitConst = (Literal.unitLit, BOMTy.unitTy)


(* FIXME: need constructor functions *)
    fun mkExp t = E_Pt(ProgPt.new(), t)
    fun mkLet (lhs, rhs, exp) = (
    	  List.app (fn x => Var.setKind (x, VK_Let rhs)) lhs;
	  mkExp(E_Let(lhs, rhs, exp)))
    fun mkStmt (lhs, rhs, exp) = (
    	  List.app (fn x => Var.setKind (x, VK_RHS rhs)) lhs;
	  mkExp(E_Stmt(lhs, rhs, exp)))
    fun mkFun(fbs, e) = let
    	  fun setKind (lambda as FB{f, params, exh, ...}) = (
		Var.setKind(f, VK_Fun lambda);
		List.app (fn x => Var.setKind(x, VK_Param)) (params @ exh))
	  in
	    List.app setKind fbs;
	    mkExp(E_Fun(fbs, e))
	  end
    fun mkCont (lambda as FB{f, params, ...},e) = (
          Var.setKind (f, VK_Cont lambda);
	  List.app (fn x=> Var.setKind(x, VK_Param)) params;
	  mkExp(E_Cont(lambda, e)))
    fun mkIf arg = mkExp(E_If arg)
    fun mkCase arg = mkExp(E_Case arg)
    fun mkApply arg = mkExp(E_Apply arg)
    fun mkThrow arg = mkExp(E_Throw arg)
    fun mkRet arg = mkExp(E_Ret arg)
    fun mkHLOp arg = mkExp(E_HLOp arg)

    fun mkCFun arg = (
	  Var.setKind(#var arg, VK_Extern(#name arg));
	  CFunctions.CFun arg)

    fun mkModule (name, externs, body as FB{params, exh, ...}) = (
	  List.app (fn x => Var.setKind(x, VK_Param)) (params @ exh);
	  List.app
	    (fn (CFunctions.CFun{var, name, ...}) => Var.setKind(var, VK_Extern name))
	      externs;
	  MODULE{name = name, externs = externs, body = body})

  (* for sequences of Stms *)
    fun mkStmts ([], e) = e
      | mkStmts ((lhs, rhs)::r, e) = mkStmt(lhs, rhs, mkStmts(r, e))

  end
