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

  (* apply a substitution to a RHS *)
    val substRHS : (subst * BOM.rhs) -> BOM.rhs

  (* beta-reduce a lambda application; the resulting term will have
   * fresh bound variables.
   *)
    val applyLambda : (BOM.lambda * BOM.var list * BOM.var list) -> BOM.exp

  (* create a copy of a BOM term with fresh bound variables *)
    val copyLambda : BOM.lambda -> BOM.lambda

  end = struct

    structure B = BOM
    structure BV = BOM.Var
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
    fun extend' (s, [], []) = s
      | extend' (s, x::xs, y::ys) = extend'(VMap.insert(s, x, y), xs, ys)
      | extend' (s, _, _) = raise Fail "BOMUtil.extend': unequal lists"

  (* apply a substitution to a RHS term *)
    fun substRHS (s, rhs) = (case rhs
	   of B.E_Const _ => rhs
	    | B.E_Cast(ty, x) => B.E_Cast(ty, subst s x)
	    | B.E_Select(i, x) => B.E_Select(i, subst s x)
	    | B.E_Update(i, x, y) => B.E_Update(i, subst s x, subst s y)
	    | B.E_AddrOf(i, x) => B.E_AddrOf(i, subst s x)
	    | B.E_Alloc(ty, args) => B.E_Alloc(ty, subst'(s, args))
	    | B.E_Wrap x => B.E_Wrap(subst s x)
	    | B.E_Unwrap x => B.E_Unwrap(subst s x)
	    | B.E_Prim p => B.E_Prim(PrimUtil.map (subst s) p)
	    | B.E_DCon(dc, args) => B.E_DCon(dc, subst'(s, args))
	    | B.E_CCall(f, args) => B.E_CCall(subst s f, subst'(s, args))
	    | B.E_HostVProc => rhs
	    | B.E_VPLoad(n, x) => B.E_VPLoad(n, subst s x)
	    | B.E_VPStore(n, x, y) => B.E_VPStore(n, subst s x, subst s y)
	  (* end case *))

    fun freshVar (s, x) = let
	  val x' = BV.copy x
	  in
	    (extend(s, x, x'), x')
	  end
    fun freshVars (s, xs) = let
	  fun fresh (x::xs, s, xs') = let
		val x' = BV.copy x
		in
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
   * fresh bound variables.
   *)
    fun applyLambda (B.FB{f, params, exh, body}, args, rets) = let
	  val s = extend' (empty, params, args)
		    handle _ => raise Fail("param/arg mismatch in application of " ^ BV.toString f)
	  val s = extend' (s, exh, rets)
		    handle _ => raise Fail("exh/rets mismatch in application of " ^ BV.toString f)
	  in
	    copyExp' (s, body)
	  end

  (* create a copy of a BOM term with fresh bound variables *)
    fun copyLambda fb = #2 (copyOneLambda (empty, fb))

  end
