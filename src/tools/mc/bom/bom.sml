(* bom.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOM =
  struct

    datatype data_con = datatype BOMTy.data_con
    datatype dcon_rep = datatype BOMTy.dcon_rep

    type ty = BOMTy.ty
    type hlop = HLOp.hlop

    type offset = IntInf.int

    datatype exp = E_Pt of (ProgPt.ppt * term)

    and term
      = E_Let of (var list * exp * exp)
      | E_Stmt of (var list * rhs * exp)
      | E_Fun of (lambda list * exp)
      | E_Cont of (lambda * exp)
      | E_If of (cond * exp * exp)
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
      | E_AddrOf of (int * var)			(* return address of i'th field (zero-based) *)
      | E_Alloc of (ty * var list)		(* allocation in local heap *)
      | E_Promote of var			(* promotion of object to global heap *)
      | E_Prim of prim
      | E_DCon of (data_con * var list)		(* data constructor; the argument list is empty for *)
						(* nullary constructors *)
      | E_CCall of (var * var list)		(* foreign-function calls *)
    (* VProc operations *)
      | E_HostVProc				(* gets the hosting VProc *)
      | E_VPLoad of (offset * var)		(* load a value from the given byte *)
						(* offset in the vproc structure *)
      | E_VPStore of (offset * var * var)	(* store a value at the given byte *)
						(* offset in the vproc structure *)
      | E_VPAddr of (offset * var)		(* address of given byte offset *)
						(* in the vproc structure *)

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

    and var_kind
      = VK_None
      | VK_Let of exp
      | VK_RHS of rhs
      | VK_Param
      | VK_Fun of lambda
      | VK_Cont of lambda
      | VK_Extern of string

    (* rewrite pattern *)
    and rw_pattern 
      = RW_HLOpApply of (hlop * rw_pattern list)        (* application of a hlop *)
      | RW_Prim of (var * rw_pattern list)              (* application of a prim-op or data constructor  *)
      | RW_Const of (Literal.literal * ty)
      | RW_Var of var

    and rewrite = Rewrite of { label  : Atom.atom,         (* hlop rewrite rule *)
			       lhs    : rw_pattern,
			       rhs    : rw_pattern,
			       weight : IntInf.int }		         

    withtype var = (var_kind, ty) VarRep.var_rep
         and cond = var Prim.cond
         and prim = var Prim.prim
	 and const = (Literal.literal * ty)

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
	      val tyToString = BOMTyUtil.toString
	    end)
	in
	open V
      (* application counts for functions *)
	local
	  val {clrFn, getFn, peekFn, ...} = newProp (fn _ => ref 0)
	in
	val appCntRef = getFn
	val appCntRmv = clrFn
	fun appCntOf v = (case peekFn v of NONE => 0 | (SOME ri) => !ri)
	fun combineAppUseCnts (x as VarRep.V{useCnt=ux, ...}, y as VarRep.V{useCnt=uy, ...}) = (
	      ux := !ux + !uy;
	      case peekFn y
	       of (SOME ry) => let
		    val rx = appCntRef x
		    in
		      rx := !rx + !ry
		    end
		| NONE => ()
	      (* end case *))
      (* string representation that includes counts *)
	val toString = fn x => (case peekFn x
	       of NONE => concat[toString x, "#", Int.toString(useCount x)]
		| SOME r => concat[
		      toString x, "#", Int.toString(useCount x),
		      ".", Int.toString(!r)
		    ]
	      (* end case *))
	end (* local val ... *)
      (* mapping from functions to the HLOp that they define *)
	val {clrFn = clrHLOp, peekFn = hlop, setFn = setHLOp, ...} =
	      newProp (fn _ => ((raise Fail "no HLOp") : hlop))
	fun isHLOp f = Option.isSome(hlop f)
	end (* local structure V = ... *)
      end 

    datatype module = MODULE of {
	name : Atom.atom,
	externs : var CFunctions.c_fun list,
	hlops : var list,		    (* the names of the functions that *)
					    (* are define HLOps *)
	rewrites : rewrite list,
	body : lambda
      }

    val unitConst = (Literal.unitLit, BOMTy.unitTy)

  (* wrapped raw values are stored in tuples *)
    fun wrap x = E_Alloc(BOMTyUtil.wrap(Var.typeOf x), [x])
    fun unwrap x = E_Select(0, x)

(* FIXME: need constructor functions *)

  (* mkExp : term -> exp *)
    fun mkExp t = E_Pt(ProgPt.new(), t)

    fun mkLet (lhs, rhs, exp) = (
    	  List.app (fn x => Var.setKind (x, VK_Let rhs)) lhs;
	  mkExp(E_Let(lhs, rhs, exp)))
    fun mkStmt (lhs, rhs, exp) = (
    	  List.app (fn x => Var.setKind (x, VK_RHS rhs)) lhs;
	  mkExp(E_Stmt(lhs, rhs, exp)))
    fun mkStmts ([], exp) = exp
      | mkStmts ((lhs, rhs)::r, exp) = mkStmt(lhs, rhs, mkStmts(r, exp))

    local
    fun setLambdaKind (lambda as FB{f, params, exh, ...}) = (
	  Var.setKind(f, VK_Fun lambda);
	  List.app (fn x => Var.setKind(x, VK_Param)) (params @ exh))
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
    end

    fun mkCont (lambda as FB{f, params, ...},e) = (
          Var.setKind (f, VK_Cont lambda);
	  List.app (fn x=> Var.setKind(x, VK_Param)) params;
	  mkExp(E_Cont(lambda, e)))
    fun mkIf arg = mkExp(E_If arg)

  (* mkCase : var * (pat * exp) list * exp option -> exp *)
    fun mkCase arg = mkExp(E_Case arg)

    fun mkApply arg = mkExp(E_Apply arg)
    fun mkThrow arg = mkExp(E_Throw arg)
    fun mkRet arg = mkExp(E_Ret arg)
    fun mkHLOp arg = mkExp(E_HLOp arg)

    fun mkCFun arg = (
	  Var.setKind(#var arg, VK_Extern(#name arg));
	  CFunctions.CFun arg)

  (* mkModule : Atom.atom * var CFunctions.c_fun list * rewrite list * lambda -> module *)
    fun mkModule (name, externs, hlops, rewrites, body as FB{params, exh, ...}) = (
	  List.app (fn x => Var.setKind(x, VK_Param)) (params @ exh);
	  List.app
	    (fn (CFunctions.CFun{var, name, ...}) => Var.setKind(var, VK_Extern name))
	      externs;
	  MODULE{name = name, externs = externs, hlops = hlops, rewrites = rewrites, body = body})

  end
