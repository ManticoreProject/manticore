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

  (* return the type of a BOM term *)
    val typeOfExp : BOM.exp -> BOM.ty list
    val typeOfRHS : BOM.rhs -> BOM.ty list
    val typeOfPrim : BOM.prim -> BOM.ty list

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
      | varsOfRHS (B.E_Prim p) = PrimUtil.varsOf p
      | varsOfRHS (B.E_DCon(_, args)) = args
      | varsOfRHS (B.E_CCall(f, args)) = f :: args
      | varsOfRHS (B.E_HostVProc) = []
      | varsOfRHS (B.E_VPLoad(n, x)) = [x]
      | varsOfRHS (B.E_VPStore(n, x, y)) = [x, y]

  (* apply a substitution to a RHS term *)
    fun substRHS (s, rhs) = (case rhs
	   of B.E_Const _ => rhs
	    | B.E_Cast(ty, x) => B.E_Cast(ty, subst s x)
	    | B.E_Select(i, x) => B.E_Select(i, subst s x)
	    | B.E_Update(i, x, y) => B.E_Update(i, subst s x, subst s y)
	    | B.E_AddrOf(i, x) => B.E_AddrOf(i, subst s x)
	    | B.E_Alloc(ty, args) => B.E_Alloc(ty, subst'(s, args))
	    | B.E_Prim p => B.E_Prim(PrimUtil.map (subst s) p)
	    | B.E_DCon(dc, args) => B.E_DCon(dc, subst'(s, args))
	    | B.E_CCall(f, args) => B.E_CCall(subst s f, subst'(s, args))
	    | B.E_HostVProc => rhs
	    | B.E_VPLoad(n, x) => B.E_VPLoad(n, subst s x)
	    | B.E_VPStore(n, x, y) => B.E_VPStore(n, subst s x, subst s y)
	  (* end case *))

  (* apply a substitution to an expression *)
    fun substExp (s, e) = let
	  fun substE (B.E_Pt(_, e)) = (case e
		 of B.E_Let(xs, e1, e2) => B.mkLet(xs, substE e1, substE e2)
		  | B.E_Stmt(xs, rhs, e) => B.mkStmt(xs, substRHS (s, rhs), substE e)
		  | B.E_Fun(fbs, e) => B.mkFun(List.map substFB fbs, substE e)
		  | B.E_Cont(fb, e) => B.mkCont(substFB fb, substE e)
		  | B.E_If(x, e1, e2) => B.mkIf(subst s x, substE e1, substE e2)
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
		  B.FB{f=f, params=params, exh=exh, body=copyExp'(s, body)}
		end
	  in
	    (s, doBody)
	  end

    and copyOneLambda (s, fb) = let
	  val (s, doBody) = copyLambda' (s, fb)
	  in
	    (s, doBody s)
	  end

    and copyExp' (s, B.E_Pt(_, t)) = (case t
	   of B.E_Let(lhs, e1, e2) => let
		val (s', lhs) = freshVars(s, lhs)
		in
		  B.mkLet(lhs, copyExp' (s, e1), copyExp' (s', e2))
		end
	    | B.E_Stmt(lhs, rhs, e) => let
		val (s', lhs) = freshVars(s, lhs)
		in
		  B.mkStmt(lhs, substRHS (s, rhs), copyExp' (s', e))
		end
	    | B.E_Fun(fbs, e) => let
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
		  B.mkFun(fbs, copyExp'(s, e))
		end
	    | B.E_Cont(fb, e) => let
		val (s, fb) = copyOneLambda(s, fb)
		in
		  B.mkCont(fb, copyExp'(s, e))
		end
	    | B.E_If(x, e1, e2) => B.mkIf(subst s x, copyExp'(s, e1), copyExp'(s, e2))
	    | B.E_Case(x, cases, dflt) => let
		fun copyCase (B.P_DCon(dc, args), e) = let
		      val (s, args) = freshVars(s, args)
		      in
			(B.P_DCon(dc, args), copyExp'(s, e))
		      end
		  | copyCase (p, e) = (p, copyExp'(s, e))
		in
		  B.mkCase(subst s x,
		    List.map copyCase cases,
		    Option.map (fn e => copyExp' (s, e)) dflt)
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
	  val s = extend' (empty, params, args)
		    handle _ => raise Fail("param/arg mismatch in application of " ^ BV.toString f)
	  val s = extend' (s, exh, rets)
		    handle _ => raise Fail("exh/rets mismatch in application of " ^ BV.toString f)
	  fun adjust (arg as VarRep.V{useCnt, ...}, param) = (
		BV.combineAppUseCnts (arg, param);
		useCnt := !useCnt - 1)
	  in
	    ListPair.app adjust (args, params);
	    ListPair.app adjust (rets, exh);
	    copyExp' (s, body)
	  end

  (* create a copy of a BOM term with fresh bound variables *)
    fun copyLambda fb = #2 (copyOneLambda (empty, fb))

    local
      structure PTy = PrimTyFn (
	struct
	  structure V = BV
	  val bool = BTy.boolTy
	  val raw = BTy.T_Raw
	end)
    in
  (* return the type of a BOM term *)
    fun typeOfPrim prim = [PTy.typeOf prim]

    fun typeOfRHS (B.E_Const(_, ty)) = [ty]
      | typeOfRHS (B.E_Cast(ty, _)) = [ty]
      | typeOfRHS (B.E_Select(i, x)) = [BTy.select(BV.typeOf x, i)]
      | typeOfRHS (B.E_Update _) = []
      | typeOfRHS (B.E_AddrOf(i, x)) = [BTy.T_Addr(BTy.select(BV.typeOf x, i))]
      | typeOfRHS (B.E_Alloc(ty, _)) = [ty]
      | typeOfRHS (B.E_Prim p) = typeOfPrim p
      | typeOfRHS (B.E_DCon(dc, _)) = [BTy.typeOfDCon dc]
      | typeOfRHS (B.E_CCall(cf, _)) = let
	  val BTy.T_CFun(CFunctions.CProto(cty, _, _)) = BV.typeOf cf
	  in
	    BTy.ctypeToBOM cty
	  end
      | typeOfRHS (B.E_HostVProc) = [BTy.T_VProc]
      | typeOfRHS (B.E_VPLoad _) = [BTy.T_Any]
      | typeOfRHS (B.E_VPStore _) = []

    fun typeOfExp (B.E_Pt(_, t)) = (case t
	   of (B.E_Let(_, _, e)) => typeOfExp e
	    | (B.E_Stmt(_, _, e)) => typeOfExp e
	    | (B.E_Fun(_, e)) => typeOfExp e
	    | (B.E_Cont(_, e)) => typeOfExp e
	    | (B.E_If(_, e, _)) => typeOfExp e
	    | (B.E_Case(_, (_, e)::_, _)) => typeOfExp e
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
    fun expToString _ = "<exp>"	(* FIXME *)
    fun rhsToString _ = "<rhs>"	(* FIXME *)

  end
