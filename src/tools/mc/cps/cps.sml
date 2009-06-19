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

    datatype exp = Exp of (ProgPt.ppt * term)

    and term
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
      | Alloc of (ty * var list)	(* local-heap allocation *)
      | Promote of var			(* promote a heap object to the global heap *)
      | Prim of prim
      | CCall of (var * var list)
    (* VProc operations *)
      | HostVProc			(* gets the hosting VProc *)
      | VPLoad of (offset * var)	(* load a value from the given byte offset *)
					(* in the vproc structure *)
      | VPStore of (offset * var * var)	(* store a value at the given byte offset *)
					(* in the vproc structure *)
      | VPAddr of (offset * var)	(* address of given byte offset in the vproc *)
					(* structure *)

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
      | VK_CFun of c_fun

    withtype var = (var_kind, ty) VarRep.var_rep
         and prim = var Prim.prim
	 and c_fun = var CFunctions.c_fun

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
      | varKindToString (VK_CFun _) = "CFun"

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
	fun isExtern (VarRep.V{kind = ref(VK_CFun _), ...}) = true
	  | isExtern _ = false
      (* application counts for functions and continuations *)
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
	val toString = fn x => (case peekFn x
	       of NONE => concat[toString x, "#", Int.toString(useCount x)]
		| SOME r => concat[toString x, "#", Int.toString(useCount x), ".", Int.toString(!r)]
	      (* end case *))
	end (* local open V ... *)
	end (* local *)
      end

  (* representation of true and false *)
    val trueRHS = Const(Literal.trueLit, CPSTy.boolTy)
    val falseRHS = Const(Literal.falseLit, CPSTy.boolTy)

  (* representation of unit *)
    val unitRHS = Const(Literal.unitLit, CPSTy.unitTy)

  (* smart constructors; these enforce the variable kind invariant and should be
   * used to construct terms.
   *)
    fun mkExp t = Exp(ProgPt.new(), t)
    fun mkLet (lhs, rhs, exp) = (
	  List.app (fn x => Var.setKind(x, VK_Let rhs)) lhs;
	  mkExp(Let(lhs, rhs, exp)))
    fun mkLambda (lambda as FB{f, params, rets, ...}) = (
	  Var.setKind(f, VK_Fun lambda);
	  List.app (fn x => Var.setKind(x, VK_Param lambda)) params;
	  List.app (fn x => Var.setKind(x, VK_Param lambda)) rets;
	  lambda)
    fun mkFun (fbs, e) = mkExp(Fun(List.map mkLambda fbs, e))
    fun mkCont (lambda as FB{f, params, rets=[], ...}, e) = (
	  Var.setKind(f, VK_Cont lambda);
	  List.app (fn x => Var.setKind(x, VK_Param lambda)) params;
	  mkExp(Cont(lambda, e)))

    fun mkIf arg = mkExp(If arg)
    fun mkSwitch arg = mkExp(Switch arg)
    fun mkApply arg = mkExp(Apply arg)
    fun mkThrow arg = mkExp(Throw arg)

    fun mkCFun arg = let
	  val cf = CFunctions.CFun arg
	  in
	    Var.setKind(#var arg, VK_CFun cf);
	    cf
	  end

  end
