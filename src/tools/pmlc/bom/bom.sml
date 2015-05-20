(* bom.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOM : sig

    type ty_var = BOMTyVar.t
    type ty = BOMTy.t
    type var = BOMVar.t
    type data_con = BOMDataCon.t
    type hlop = HLOp.t
    type cond = var Prim.cond
    type prim = var Prim.prim
    type const = (Literal.literal * ty)
    type c_fun = var CFunctions.c_fun

    type vp_field = IntInf.int

    datatype exp = E_Pt of (ProgPt.ppt * term)

    and term
      = E_Let of (var list * exp * exp)
      | E_Stmt of (var list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (cond * exp * exp)
      | E_Case of (var * (pat * exp) list * exp option)
      | E_Typecase of (ty_var * (ty * exp) list * exp option)  (* only inside HLOp definitions *)
      | E_Apply of (var * var list * var list)
      | E_Throw of (var * var list)
      | E_Ret of var list
      | E_HLOp of (hlop * var list * var list)	(* application of high-level operator *)

    and rhs
      = E_Prim of prim
      | E_Alloc of (ty * var list)		(* allocation in local heap *)
      | E_DCon of (data_con * var list)		(* data constructor; the argument list is empty for *)
						(* nullary constructors *)
      | E_Select of (int * var)			(* select i'th field (zero-based) *)
      | E_Update of (int * var * var)		(* update i'th field (zero-based) *)
      | E_AddrOf of (int * var)			(* return address of i'th field (zero-based) *)
      | E_Cast of (ty * var)			(* deprecated *)
      | E_Promote of var			(* promotion of object to global heap *)
      | E_CCall of (var * var list)		(* foreign-function calls *)
      | E_HostVProc				(* gets the hosting VProc *)
      | E_VPLoad of (vp_field * var)		(* load a value from the given byte *)
						(* offset in the vproc structure *)
      | E_VPStore of (vp_field * var * var)	(* store a value at the given byte *)
						(* offset in the vproc structure *)
      | E_VPAddr of (vp_field * var)		(* address of given byte offset *)
						(* in the vproc structure *)
      | E_Const of const

    and lambda = FB of {	  	    (* function/continuation abstraction *)
	  f : var,				(* function name *)
	  params : var list,			(* parameters *)
	  exh : var list,			(* exception continuation *)
	  body : exp				(* function body *)
	}

    and pat			  	    (* simple, one-level, patterns *)
      = P_DCon of data_con * var list		(* data constructor; the argument *)
						(* list is empty for *)
						(* nullary constructors *)
      | P_Const of const

    datatype program = PROGRAM of {
	exnTyc : BOMTyc.t,			(* the exception datatype *)
	dataTycs : BOMTyc.t list,		(* other datatypes *)
	hlops : hlop list,			(* the HLOps *)
	externs : var CFunctions.c_fun list,	(* C functions that are referenced by the program *)
	body : lambda				(* the program body *)
      }

  (* smart constructors *)
    val mkLet : var list * exp * exp -> exp
    val mkStmt : var list * rhs * exp -> exp
    val mkStmts : (var list * rhs) list * exp -> exp
    val mkLambda : {f : var, params : var list, exh : var list, body : exp} -> lambda
    val mkFun : lambda list * exp -> exp
    val mkCont : lambda * exp -> exp
    val mkIf : cond * exp * exp -> exp
    val mkCase : var * (pat * exp) list * exp option -> exp
    val mkTypecase : ty_var * (ty * exp) list * exp option -> exp
    val mkApply : var * var list * var list -> exp
    val mkThrow : var * var list -> exp
    val mkRet : var list -> exp
    val mkHLOp : hlop * var list * var list -> exp

    val mkCFun : {
	    var : var,
	    name : string,
	    retTy : CFunctions.c_type,
	    argTys : CFunctions.c_type list,
	    varArg : bool,
	    attrs : CFunctions.attribute list
	  } -> c_fun

  end = struct

    type ty_var = BOMTyVar.t
    type ty = BOMTy.t
    type var = BOMVar.t
    type data_con = BOMDataCon.t
    type hlop = HLOp.t
    type cond = var Prim.cond
    type prim = var Prim.prim
    type const = (Literal.literal * ty)
    type c_fun = var CFunctions.c_fun
    type vp_field = BOMRep.vp_field

    datatype exp = datatype BOMRep.exp
    datatype term = datatype BOMRep.term
    datatype rhs = datatype BOMRep.rhs
    datatype lambda = datatype BOMRep.lambda
    datatype pat = datatype BOMRep.pat
    datatype program = datatype BOMRep.program

  (* mkExp : term -> exp *)
    fun mkExp t = E_Pt(ProgPt.new(), t)

    fun mkLet (lhs, rhs, exp) = (
    	  List.app (fn x => BOMVar.setKind (x, BOMVar.VK_Let rhs)) lhs;
	  mkExp(E_Let(lhs, rhs, exp)))
    fun mkStmt (lhs, rhs, exp) = (
    	  List.app (fn x => BOMVar.setKind (x, BOMVar.VK_RHS rhs)) lhs;
	  mkExp(E_Stmt(lhs, rhs, exp)))
    fun mkStmts ([], exp) = exp
      | mkStmts ((lhs, rhs)::r, exp) = mkStmt(lhs, rhs, mkStmts(r, exp))

    local
    fun setLambdaKind (lambda as FB{f, params, exh, ...}) = (
	  BOMVar.setKind(f, BOMVar.VK_Fun lambda);
	  List.app (fn x => BOMVar.setKind(x, BOMVar.VK_Param)) (params @ exh))
    in
    fun mkLambda {f, params, exh, body} = let
	  val l = FB{f=f, params=params, exh=exh, body=body}
	  in
	    setLambdaKind l;
	    l
	  end
    fun mkFun (fbs, e) = (
	  List.app setLambdaKind fbs;
	  mkExp(E_Fun(fbs, e)))
    end (* local *)

    fun mkCont (lambda as FB{f, params, ...},e) = (
          BOMVar.setKind (f, BOMVar.VK_Cont lambda);
	  List.app (fn x => BOMVar.setKind(x, BOMVar.VK_Param)) params;
	  mkExp(E_Cont(lambda, e)))
    fun mkIf arg = mkExp(E_If arg)
    fun mkCase arg = mkExp(E_Case arg)
    fun mkTypecase arg = mkExp(E_Typecase arg)
    fun mkApply arg = mkExp(E_Apply arg)
    fun mkThrow arg = mkExp(E_Throw arg)
    fun mkRet arg = mkExp(E_Ret arg)
    fun mkHLOp arg = mkExp(E_HLOp arg)

    fun mkCFun arg = let
	  val cf = CFunctions.CFun arg
	  in
	    BOMVar.setKind(#var arg, BOMVar.VK_CFun cf);
	    cf
	  end

  end
