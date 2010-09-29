(* bom-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMUtil : sig

  (* substitutions from variables to variables *)
    type subst = BOM.var BOM.Var.Map.map

    val empty : subst
    val singleton : (BOM.var * BOM.var) -> subst
    val subst : subst -> BOM.var -> BOM.var
    val subst' : (subst * BOM.var list) -> BOM.var list
    val extend : (subst * BOM.var * BOM.var) -> subst
    val extend' : (subst * BOM.var list * BOM.var list) -> subst

  (* return the variables used in a RHS term *)
    val varsOfRHS : BOM.rhs -> BOM.var list

  (* apply a substitution to a RHS *)
    val substRHS : (subst * BOM.rhs) -> BOM.rhs

  (* apply a substitution to an expression *)
    val substExp : (subst * BOM.exp) -> BOM.exp

  (* apply a function to the variables of a RHS *)
    val appRHS : (BOM.var -> unit) -> BOM.rhs -> unit

  (* beta-reduce a lambda application; the resulting term will have
   * fresh bound variables.
   *)
    val applyLambda : (BOM.lambda * BOM.var list * BOM.var list) -> BOM.exp

  (* create a copy of a BOM term with fresh bound variables *)
    val copyLambda : BOM.lambda -> BOM.lambda

  (* create a copy of a list of mutually recursive functions *)
    val copyLambdas : BOM.lambda list -> BOM.lambda list

  (* copy an expression creating fresh local variables and renaming global variables
   * according to the given substitution.
   *)
    val copyExp : (subst * BOM.exp) -> BOM.exp

  (* make a raw int expression from an int *)
    val rawInt : int -> BOM.rhs

  (* make a case for testing booleans (i.e., an if-the-else) *)
    val mkBoolCase : (BOM.var * BOM.exp * BOM.exp) -> BOM.exp

  (* return the type of a BOM term *)
    val typeOfExp : BOM.exp -> BOM.ty list
    val typeOfRHS : BOM.rhs -> BOM.ty list
    val typeOfPrim : BOM.prim -> BOM.ty option
    val signOfPrim : BOM.prim -> (BOM.ty list * BOM.ty option)
    val condArgTys : BOM.cond -> BOM.ty list

  (* for debugging output *)
    val expToString : BOM.exp -> string
    val rhsToString : BOM.rhs -> string

  end = struct

    structure B = BOM
    structure BV = BOM.Var
    structure BTy = BOMTy
    structure VMap = BV.Map

  (* substitutions from variables to variables *)
    type subst = B.var VMap.map

    val empty : subst = VMap.empty
    val singleton : (BOM.var * BOM.var) -> subst = VMap.singleton
    fun subst s x = (case VMap.find(s, x)
	   of NONE => x
	    | SOME y => y
	  (* end case *))
    fun subst' (s, l) = List.map (subst s) l
    val extend : (subst * BOM.var * BOM.var) -> subst = VMap.insert
    
    (* extend' : subst * BOM.var list * BOM.var list -> subst *)
    (* Pre: The lists of variables are of equal length. *)
    fun extend' (s, [], []) = s
      | extend' (s, x::xs, y::ys) = extend'(VMap.insert(s, x, y), xs, ys)
      | extend' (s, _, _) = raise Fail "BOMUtil.extend': unequal lists"

  (* return the variables used in a RHS term *)
    fun varsOfRHS (B.E_Const _) = []
      | varsOfRHS (B.E_Cast(_, x)) = [x]
      | varsOfRHS (B.E_Select(_, x)) = [x]
      | varsOfRHS (B.E_Update(_, x, y)) = [x, y]
      | varsOfRHS (B.E_AddrOf(_, x)) = [x]
      | varsOfRHS (B.E_Alloc(_, args)) = args
      | varsOfRHS (B.E_AllocSpecial(_, args)) = args
      | varsOfRHS (B.E_Promote x) = [x]
      | varsOfRHS (B.E_Prim p) = PrimUtil.varsOf p
      | varsOfRHS (B.E_DCon(_, args)) = args
      | varsOfRHS (B.E_CCall(f, args)) = f :: args
      | varsOfRHS (B.E_HostVProc) = []
      | varsOfRHS (B.E_VPLoad(n, x)) = [x]
      | varsOfRHS (B.E_VPStore(n, x, y)) = [x, y]
      | varsOfRHS (B.E_VPAddr(n, x)) = [x]

  (* apply a substitution to a RHS term *)
    fun substRHS (s, rhs) = (case rhs
	   of B.E_Const _ => rhs
	    | B.E_Cast(ty, x) => B.E_Cast(ty, subst s x)
	    | B.E_Select(i, x) => B.E_Select(i, subst s x)
	    | B.E_Update(i, x, y) => B.E_Update(i, subst s x, subst s y)
	    | B.E_AddrOf(i, x) => B.E_AddrOf(i, subst s x)
	    | B.E_Alloc(ty, args) => B.E_Alloc(ty, subst'(s, args))
	    | B.E_AllocSpecial(ty, args) => B.E_AllocSpecial(ty, subst'(s, args))
	    | B.E_Promote x => B.E_Promote(subst s x)
	    | B.E_Prim p => B.E_Prim(PrimUtil.map (subst s) p)
	    | B.E_DCon(dc, args) => B.E_DCon(dc, subst'(s, args))
	    | B.E_CCall(f, args) => B.E_CCall(subst s f, subst'(s, args))
	    | B.E_HostVProc => rhs
	    | B.E_VPLoad(n, x) => B.E_VPLoad(n, subst s x)
	    | B.E_VPStore(n, x, y) => B.E_VPStore(n, subst s x, subst s y)
	    | B.E_VPAddr(n, x) => B.E_VPAddr(n, subst s x)
	  (* end case *))

  (* apply a substitution to an expression *)
    fun substExp (s, e) = let
	  fun substE (B.E_Pt(_, e)) = (case e
		 of B.E_Let(xs, e1, e2) => B.mkLet(xs, substE e1, substE e2)
		  | B.E_Stmt(xs, rhs, e) => B.mkStmt(xs, substRHS (s, rhs), substE e)
		  | B.E_Fun(fbs, e) => B.mkFun(List.map substFB fbs, substE e)
		  | B.E_Cont(fb, e) => B.mkCont(substFB fb, substE e)
		  | B.E_If(cond, e1, e2) =>
		      B.mkIf(CondUtil.map (subst s) cond, substE e1, substE e2)
		  | B.E_Case(x, cases, dflt) =>
		      B.mkCase(subst s x,
			List.map (fn (p, e) => (p, substE e)) cases,
			Option.map substE dflt)
		  | B.E_Apply(f, args, rets) =>
		      B.mkApply(subst s f, subst'(s, args), subst'(s, rets))
		  | B.E_Throw(k, args) =>
		      B.mkThrow(subst s k, subst'(s, args))
		  | B.E_Ret xs => B.mkRet(subst'(s, xs))
		  | B.E_HLOp(hlop, args, rets) =>
		      B.mkHLOp(hlop, subst'(s, args), subst'(s, rets))
		(* end case *))
	  and substFB (B.FB{f, params, exh, body}) =
		B.FB{f=f, params=params, exh=exh, body=substE body}
	  in
	    substE e
	  end

  (* apply a function to the variables of a RHS *)
    fun appRHS f rhs = List.app f (varsOfRHS rhs)

    fun freshVar (s, x) = let
	  val x' = BV.copy x
	  in
	    BV.combineAppUseCnts (x', x);
	    (extend(s, x, x'), x')
	  end
    fun freshVars (s, xs) = let
	  fun fresh (x::xs, s, xs') = let
		val x' = BV.copy x
		in
		  BV.combineAppUseCnts (x', x);
		  fresh(xs, extend(s, x, x'), x'::xs')
		end
	    | fresh ([], s, xs') = (s, List.rev xs')
	  in
	    fresh (xs, s, [])
	  end

  (* copy a lambda term; this is done as a staged operation, since we must
   * handle mutually recursive functions.
   *)
    fun copyLambda' (s, B.FB{f, params, exh, body}) = let
	  val (s, f) = freshVar (s, f)
	  fun doBody s = let
		val (s, params) = freshVars (s, params)
		val (s, exh) = freshVars (s, exh)
		in
		  B.FB{f=f, params=params, exh=exh, body=copyExp(s, body)}
		end
	  in
	    (s, doBody)
	  end

    and copyOneLambda (s, fb) = let
	  val (s, doBody) = copyLambda' (s, fb)
	  in
	    (s, doBody s)
	  end

    and copyFBs (s, fbs) = let
	(* first pass creates fresh function names and gives a list of doBody
	 * functions in reverse order.
	 *)
	  val (s, doBodies) = List.foldl
		(fn (fb, (s, doBodies)) => let val (s, doBody) = copyLambda' (s, fb)
		  in
		    (s, doBody::doBodies)
		  end)
		  (s, []) fbs
	(* second pass creates the copies and reverses the list back to its original
	 * order.
	 *)
	  val fbs = List.foldl (fn (doBody, fbs) => doBody s :: fbs) [] doBodies
	  in
	    (s, fbs)
	  end

    and copyExp (s, B.E_Pt(_, t)) = (case t
	   of B.E_Let(lhs, e1, e2) => let
		val (s', lhs) = freshVars(s, lhs)
		in
		  B.mkLet(lhs, copyExp (s, e1), copyExp (s', e2))
		end
	    | B.E_Stmt(lhs, rhs, e) => let
		val (s', lhs) = freshVars(s, lhs)
		in
		  B.mkStmt(lhs, substRHS (s, rhs), copyExp (s', e))
		end
	    | B.E_Fun(fbs, e) => let
		val (s, fbs) = copyFBs (s, fbs)
		in
		  B.mkFun(fbs, copyExp(s, e))
		end
	    | B.E_Cont(fb, e) => let
		val (s, fb) = copyOneLambda(s, fb)
		in
		  B.mkCont(fb, copyExp(s, e))
		end
	    | B.E_If(cond, e1, e2) =>
		B.mkIf(CondUtil.map (subst s) cond, copyExp(s, e1), copyExp(s, e2))
	    | B.E_Case(x, cases, dflt) => let
		fun copyCase (B.P_DCon(dc, args), e) = let
		      val (s, args) = freshVars(s, args)
		      in
			(B.P_DCon(dc, args), copyExp(s, e))
		      end
		  | copyCase (p, e) = (p, copyExp(s, e))
		in
		  B.mkCase(subst s x,
		    List.map copyCase cases,
		    Option.map (fn e => copyExp (s, e)) dflt)
		end
	    | B.E_Apply(f, args, rets) =>
		B.mkApply(subst s f, subst'(s, args), subst'(s, rets))
	    | B.E_Throw(k, args) => B.mkThrow(subst s k, subst'(s, args))
	    | B.E_Ret args => B.mkRet(subst'(s, args))
	    | B.E_HLOp(hlop, args, rets) => B.mkHLOp(hlop, subst'(s, args), subst'(s, rets))
	  (* end case *))

  (* beta-reduce a lambda application; the resulting term will have
   * fresh bound variables.  This operation also correctly preserves the
   * census counts of the parameters and arguments (but not the function
   * name itself), assuming that the original counts are correct.
   *)
    fun applyLambda (B.FB{f, params, exh, body}, args, rets) = let
	  fun err msg = raise Fail(concat[
		  msg, " mismatch in application of ", BV.toString f, ":",
		  BOMTyUtil.toString(BV.typeOf f)
		])
	  val s = extend' (empty, params, args) handle _ => err "param/arg"
	  val s = extend' (s, exh, rets) handle _ => err "exh/rets"
	  fun adjust (arg as VarRep.V{useCnt, ...}, param) = (
		BV.combineAppUseCnts (arg, param);
		useCnt := !useCnt - 1)
	  in
	    ListPair.app adjust (args, params);
	    ListPair.app adjust (rets, exh);
	    copyExp (s, body)
	  end

  (* create a copy of a BOM term with fresh bound variables *)
    fun copyLambda fb = #2 (copyOneLambda (empty, fb))

  (* create a copy of a list of mutually recursive functions *)
    fun copyLambdas fbs = #2 (copyFBs (empty, fbs))

  (* make a case for testing booleans (i.e., an if-the-else) *)
    fun mkBoolCase (arg, trueE, falseE) = B.mkCase(arg, [
	    (B.P_DCon(BOMTyUtil.trueDC, []), trueE),
	    (B.P_DCon(BOMTyUtil.falseDC, []), falseE)
	  ], NONE)

    local
      structure PTy = PrimTyFn (
	struct
	  type ty = B.ty
	  type var = B.var
	  val typeOf = B.Var.typeOf
	  val anyTy = BTy.T_Any
	  val noTy = BTy.unitTy
	  val raw = BTy.T_Raw
	  val addr = BTy.T_Addr
	end)
    in
  (* return the type of a BOM term *)
    fun typeOfPrim prim = (case PTy.typeOf prim
	   of BTy.T_Enum(0w0) => NONE (* unitTy *)
	    | ty => SOME ty
	  (* end case *))

    fun signOfPrim p = (case PTy.signOf p
	   of (tys, BTy.T_Enum(0w0)) => (tys, NONE)
	    | (tys, ty) => (tys, SOME ty)
	  (* end case *))

    val condArgTys = PTy.condArgTys

    fun typeOfRHS (B.E_Const(_, ty)) = [ty]
      | typeOfRHS (B.E_Cast(ty, _)) = [ty]
      | typeOfRHS (B.E_Select(i, x)) = [BOMTyUtil.select(BV.typeOf x, i)]
      | typeOfRHS (B.E_Update _) = []
      | typeOfRHS (B.E_AddrOf(i, x)) = [BTy.T_Addr(BOMTyUtil.select(BV.typeOf x, i))]
      | typeOfRHS (B.E_Alloc(ty, _)) = [ty]
      | typeOfRHS (B.E_AllocSpecial(ty, _)) = [ty]
      | typeOfRHS (B.E_Promote x) = [BV.typeOf x]
      | typeOfRHS (B.E_Prim p) = (case PTy.typeOf p
	   of BTy.T_Enum(0w0) => [] (* unitTy *)
	    | ty => [ty]
	  (* end case *))
      | typeOfRHS (B.E_DCon(dc, _)) = [BOMTyUtil.typeOfDCon dc]
      | typeOfRHS (B.E_CCall(cf, _)) = let
	  val BTy.T_CFun(CFunctions.CProto(cty, _, _)) = BV.typeOf cf
	  in
	    BOMTyUtil.ctypeToBOM cty
	  end
      | typeOfRHS (B.E_HostVProc) = [BTy.T_VProc]
      | typeOfRHS (B.E_VPLoad _) = [BTy.T_Any]
      | typeOfRHS (B.E_VPStore _) = []
      | typeOfRHS (B.E_VPAddr _) = [BTy.T_Addr BTy.T_Any]

  (* rawInt : int -> B.rhs *)
    fun rawInt(n) = B.E_Const(Literal.Int (IntInf.fromInt n), BTy.T_Raw BTy.T_Int)

    fun typeOfExp (B.E_Pt(_, t)) = (case t
	   of (B.E_Let(_, _, e)) => typeOfExp e
	    | (B.E_Stmt(_, _, e)) => typeOfExp e
	    | (B.E_Fun(_, e)) => typeOfExp e
	    | (B.E_Cont(_, e)) => typeOfExp e
	    | (B.E_If(_, e, _)) => typeOfExp e
	    | (B.E_Case(_, [], SOME dflt)) => typeOfExp dflt
	    | (B.E_Case(_, (_, e)::_, _)) => typeOfExp e
	    | (B.E_Case _) => raise Fail "bogus empty case"
	    | (B.E_Apply(f, _, _)) => let
		val BTy.T_Fun(_, _, tys) = BV.typeOf f
		in
		  tys
		end
	    | (B.E_Throw _) => []
	    | (B.E_Ret xs) => List.map BV.typeOf xs
	    | (B.E_HLOp(HLOp.HLOp{sign={results, ...}, ...}, _, _)) => results
	  (* end case *))

    end (* local *)

  (* for debugging output *)

    fun v2s var = BV.toString var
    fun vl2s vars = String.concatWith "," (List.map BV.toString vars)

    (* TODO: still ugly, but good enough for most debugging. *)
    fun lamToString (B.FB{f, params, exh, body}) =
        concat["Lam(", v2s f,
               " [", vl2s params, " @ ", vl2s exh, "]",
               expToString body, ")"]
    and expToString (B.E_Pt(_, t)) = (case t
	   of (B.E_Let(vars, e1, e2)) => concat["Let(", vl2s vars, " = ", expToString e1, " in ", expToString e2, ")"]
	    | (B.E_Stmt(vars, rhs, e)) => concat["Stmt(", vl2s vars, " = ", rhsToString rhs, " in ", expToString e, ")"]
	    | (B.E_Fun(lams, e)) => concat["Fun(", String.concatWith "," (List.map lamToString lams), " in ", expToString e, ")"]
	    | (B.E_Cont(lam, e)) => concat["Cont(", lamToString lam, " in ", expToString e, ")"]
	    | (B.E_If(c, e1, e2)) => concat["If(", CondUtil.nameOf c, "[", vl2s (CondUtil.varsOf c),"] then ", expToString e1, " else ", expToString e2, ")"]
	    | (B.E_Case(v, pats, dflt)) => "Case(<undone>)"
	    | (B.E_Apply(f, args, rets)) => concat["Apply(", v2s f, "[", vl2s args, " @ ", vl2s rets, "])"]
	    | (B.E_Throw (f, args)) => concat["Throw(", v2s f, "[", vl2s args, "])"]
	    | (B.E_Ret xs) => concat["Ret(", vl2s xs, ")"]
	    | (B.E_HLOp(oper, v1s, v2s)) => "HLOP(<undone>)"
	  (* end case *))
    and rhsToString (B.E_Const (lit, ty)) = concat["Const(", Literal.toString lit, ", ", BOMTyUtil.toString ty, ")"]
      | rhsToString (B.E_Cast (ty, var)) = concat["Cast(", BOMTyUtil.toString ty, ", ", v2s var, ")"]
      | rhsToString (B.E_Select (i, var)) = concat["Select(", Int.toString i, ", ", v2s var, ")"]
      | rhsToString (B.E_Update (i, v1, v2)) = concat["Update(", Int.toString i, ", ", v2s v1, ", ", v2s v2, ")"]
      | rhsToString (B.E_AddrOf (i, var)) = concat["AddrOf(", Int.toString i, ", ", v2s var, ")"]
      | rhsToString (B.E_Alloc(ty,vars)) = concat["Alloc(", BOMTyUtil.toString ty, ", ", vl2s vars, ")"]
      | rhsToString (B.E_AllocSpecial(ty,vars)) = concat["AllocSpecial(", BOMTyUtil.toString ty, ", ", vl2s vars, ")"]
      | rhsToString (B.E_Promote v) = concat["Promote(", v2s v, ")"]
      | rhsToString (B.E_Prim p) = PrimUtil.fmt v2s p
      | rhsToString (B.E_DCon (d, vars)) = concat["DataCon(", BOMTyUtil.toString (BOMTyUtil.typeOfDCon d), ", ", String.concatWith "," (List.map v2s vars), ")"]
      | rhsToString (B.E_CCall (f, args)) = concat["CCall(", v2s f, ", [", vl2s args, "])"]
      | rhsToString (B.E_HostVProc) = "HostVProc"
      | rhsToString (B.E_VPLoad (n, x)) = concat["VPLoad(", IntInf.toString n, ", ", v2s x, ")"]
      | rhsToString (B.E_VPStore (n, x, y)) = concat["VPStore(", IntInf.toString n, v2s x,  ", ", v2s y, ")"]
      | rhsToString (B.E_VPAddr (n, x)) = concat["VPAddr(", IntInf.toString n, ", ", v2s x, ")"]

  end
