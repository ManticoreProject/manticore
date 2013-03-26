(* cps-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPSUtil : sig

    val appRHS : (CPS.var -> unit) -> CPS.rhs -> unit
    val mapRHS : (CPS.var -> CPS.var) -> CPS.rhs -> CPS.rhs
    val varsOfRHS : CPS.rhs -> CPS.var list

    val rhsToString : CPS.rhs -> string

    val countPoints : CPS.exp -> int

    val applyToBoundVars : (CPS.var -> unit) -> CPS.module -> unit

  (* create a copy of a CPS term with fresh bound variables *)
    val copyLambda : CPS.lambda -> CPS.lambda

  (* create a copy of a list of mutually recursive functions *)
    val copyLambdas : CPS.lambda list -> CPS.lambda list

  (* substitutions from variables to variables *)
    type subst = CPS.var CPS.Var.Map.map
    val empty : subst

  (* copy an expression creating fresh local variables and renaming global variables
   * according to the given substitution.
   *)
    val copyExp : (subst * CPS.exp) -> CPS.exp

  end = struct

    structure C = CPS
    structure CV = CPS.Var
    structure VMap = CV.Map

    val v2s = C.Var.toString
    fun vl2s xs = String.concatWith ", " (List.map v2s xs)
    val p2s = PrimUtil.fmt v2s

    fun appRHS f rhs = (case rhs
	   of C.Var xs => List.app f xs
	    | C.Cast(ty, x) => f x
	    | C.Const(lit, ty) => ()
	    | C.Select(i, x) => f x
	    | C.Update(i, x, y) => (f x; f y)
	    | C.AddrOf(i, x) => f x
	    | C.Alloc(_, xs) => List.app f xs
	    | C.Promote x => f x
	    | C.Prim p => PrimUtil.app f p
	    | C.CCall(cf, xs) => (f cf; List.app f xs)
	    | C.HostVProc => ()
	    | C.VPLoad(n, x) => f x
	    | C.VPStore(n, x, y) => (f x; f y)
	    | C.VPAddr(n, x) => f x
	  (* end case *))

    fun mapRHS f rhs = (case rhs
	   of C.Var xs => C.Var(List.map f xs)
	    | C.Cast(ty, x) => C.Cast(ty, f x)
	    | C.Const(lit, ty) => rhs
	    | C.Select(i, x) => C.Select(i, f x)
	    | C.Update(i, x, y) => C.Update(i, f x, f y)
	    | C.AddrOf(i, x) => C.AddrOf(i, f x)
	    | C.Alloc(ty, xs) => C.Alloc(ty, List.map f xs)
	    | C.Promote x => C.Promote(f x)
	    | C.Prim p => C.Prim(PrimUtil.map f p)
	    | C.CCall(cf, xs) => C.CCall(f cf, List.map f xs)
	    | C.HostVProc => rhs
	    | C.VPLoad(n, x) => C.VPLoad(n, f x)
	    | C.VPStore(n, x, y) => C.VPStore(n, f x, f y)
	    | C.VPAddr(n, x) => C.VPAddr(n, f x)
	  (* end case *))

    fun varsOfRHS rhs = (case rhs
	   of C.Var xs => xs
	    | C.Cast(ty, x) => [x]
	    | C.Const(lit, ty) => []
	    | C.Select(i, x) => [x]
	    | C.Update(i, x, y) => [x, y]
	    | C.AddrOf(i, x) => [x]
	    | C.Alloc(_, xs) => xs
	    | C.Promote x => [x]
	    | C.Prim p => PrimUtil.varsOf p
	    | C.CCall(cf, xs) => cf::xs
	    | C.HostVProc => []
	    | C.VPLoad(n, x) => [x]
	    | C.VPStore(n, x, y) => [x, y]
	    | C.VPAddr(n, x) => [x]
	  (* end case *))

    fun rhsToString (C.Var xs) = concat["Var(", vl2s xs, ")"]
      | rhsToString (C.Cast(ty, x)) = concat["Cast(", CPSTyUtil.toString ty, ", ", v2s x, ")"]
      | rhsToString (C.Const(lit, ty)) = concat["Const(", Literal.toString lit, ", ", CPSTyUtil.toString ty, ")"]
      | rhsToString (C.Select(i, x)) = concat["Select(", Int.toString i, ", ", v2s x, ")"]
      | rhsToString (C.Update(i, x, y)) = concat["Update(", Int.toString i, ", ", v2s x,  ", ", v2s y, ")"]
      | rhsToString (C.AddrOf(i, x)) = concat["AddrOf(", Int.toString i, ", ", v2s x, ")"]
      | rhsToString (C.Alloc(ty, xs)) = concat["Alloc(", CPSTyUtil.toString ty, ", ", vl2s xs, ")"]
      | rhsToString (C.Promote x) = concat["Promote(", v2s x, ")"]
      | rhsToString (C.Prim p) = p2s p
      | rhsToString (C.CCall(cf, xs)) = concat["CCall(", v2s cf, ", [", vl2s xs, "])"]
      | rhsToString (C.HostVProc) = "HostVProc"
      | rhsToString (C.VPLoad(n, x)) = concat["VPLoad(", IntInf.toString n, ", ", v2s x, ")"]
      | rhsToString (C.VPStore(n, x, y)) = concat["VPStore(", IntInf.toString n, v2s x,  ", ", v2s y, ")"]
      | rhsToString (C.VPAddr(n, x)) = concat["VPAddr(", IntInf.toString n, ", ", v2s x, ")"]

    fun countPoints e = let
	fun doExp (C.Exp(_, e)) = (case e
		 of C.Let(xs, e1, e2) => 2 + doExp e2
		  | C.Fun(fbs, e) => 1 + (List.foldr (op +) 0 (List.map doFB fbs)) + doExp e
		  | C.Cont(fb, e) => 1 + doFB fb + doExp e
		  | C.If(cond, e1, e2) =>
		    2 + doExp e1 + doExp e2
                  | C.Switch (x, cases, dflt) =>
		    2 + (List.foldr (op +) 0 (List.map (fn (_, e) => doExp e) cases)) +
		    (case dflt of NONE => 0
				| SOME e => doExp e)
		  | C.Apply(f, args, rets) => 1
		  | C.Throw(k, args) => 1
		(* end case *))
	  and doFB (C.FB{f, params, body, rets}) =
	      1 + doExp body
	  in
	    doExp e
	  end

  (* substitutions from variables to variables *)
    type subst = C.var VMap.map

    val empty : subst = VMap.empty
    val singleton : (C.var * C.var) -> subst = VMap.singleton
    fun subst s x = (case VMap.find(s, x)
	   of NONE => x
	    | SOME y => y
	  (* end case *))
    fun subst' (s, l) = List.map (subst s) l
    val extend : (subst * CPS.var * CPS.var) -> subst = VMap.insert

    fun extend' (s, [], []) = s
      | extend' (s, x::xs, y::ys) = extend'(VMap.insert(s, x, y), xs, ys)
      | extend' (s, _, _) = raise Fail "CPSUtil.extend': unequal lists"

  (* apply a substitution to a RHS term *)
    fun substRHS (s, rhs) = (case rhs
	   of C.Var vs => C.Var (subst' (s, vs))
	    | C.Cast(ty, x) => C.Cast(ty, subst s x)
            | C.Const _ => rhs
	    | C.Select(i, x) => C.Select(i, subst s x)
	    | C.Update(i, x, y) => C.Update(i, subst s x, subst s y)
	    | C.AddrOf(i, x) => C.AddrOf(i, subst s x)
	    | C.Alloc(ty, args) => C.Alloc(ty, subst'(s, args))
	    | C.Promote x => C.Promote(subst s x)
	    | C.Prim p => C.Prim(PrimUtil.map (subst s) p)
	    | C.CCall(f, args) => C.CCall(subst s f, subst'(s, args))
	    | C.HostVProc => rhs
	    | C.VPLoad(n, x) => C.VPLoad(n, subst s x)
	    | C.VPStore(n, x, y) => C.VPStore(n, subst s x, subst s y)
	    | C.VPAddr(n, x) => C.VPAddr(n, subst s x)
	  (* end case *))

  (* apply a substitution to an expression *)
    fun substExp (s, e) = let
	  fun substE (C.Exp(_, e)) = (case e
		 of C.Let(xs, e1, e2) => C.mkLet(xs, substRHS (s,e1), substE e2)
		  | C.Fun(fbs, e) => C.mkFun(List.map substFB fbs, substE e)
		  | C.Cont(fb, e) => C.mkCont(substFB fb, substE e)
		  | C.If(cond, e1, e2) =>
		      C.mkIf(CondUtil.map (subst s) cond, substE e1, substE e2)
                  | C.Switch (x, cases, dflt) =>
		      C.mkSwitch(subst s x,
			List.map (fn (p, e) => (p, substE e)) cases,
			Option.map substE dflt)
		  | C.Apply(f, args, rets) =>
		      C.mkApply(subst s f, subst'(s, args), subst'(s, rets))
		  | C.Throw(k, args) =>
		      C.mkThrow(subst s k, subst'(s, args))
		(* end case *))
	  and substFB (C.FB{f, params, body, rets}) =
		C.FB{f=f, params=params, body=substE body, rets=rets}
	  in
	    substE e
	  end

    fun freshVar (s, x) = let
	  val x' = CPS.Var.copy x
	  in
	    CPS.Var.combineAppUseCnts (x', x);
	    (extend(s, x, x'), x')
	  end
    fun freshVars (s, xs) = let
	  fun fresh (x::xs, s, xs') = let
		val x' = CPS.Var.copy x
		in
		  CPS.Var.combineAppUseCnts (x', x);
		  fresh(xs, extend(s, x, x'), x'::xs')
		end
	    | fresh ([], s, xs') = (s, List.rev xs')
	  in
	    fresh (xs, s, [])
	  end

  (* copy a lambda term; this is done as a staged operation, since we must
   * handle mutually recursive functions.
   *)
    fun copyLambda' (s, C.FB{f, params, rets, body}) = let
	  val (s, f) = freshVar (s, f)
	  fun doBody s = let
		val (s, params) = freshVars (s, params)
		val (s, rets) = freshVars (s, rets)
		in
		  C.FB{f=f, params=params, rets=rets, body=copyExp(s, body)}
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

    and copyExp (s, C.Exp(_, t)) = (case t
	   of C.Let(lhs, e1, e2) => let
		val (s', lhs) = freshVars(s, lhs)
		in
		  C.mkLet(lhs, substRHS (s, e1), copyExp (s', e2))
		end
	    | C.Fun(fbs, e) => let
		val (s, fbs) = copyFBs (s, fbs)
		in
		  C.mkFun(fbs, copyExp(s, e))
		end
	    | C.Cont(fb, e) => let
		val (s, fb) = copyOneLambda(s, fb)
		in
		  C.mkCont(fb, copyExp(s, e))
		end
	    | C.If(cond, e1, e2) =>
		C.mkIf(CondUtil.map (subst s) cond, copyExp(s, e1), copyExp(s, e2))
	    | C.Switch(x, cases, dflt) => let
		fun copyCase (tag, e) = (tag, copyExp (s,e))
		in
		  C.mkSwitch(subst s x,
		    List.map copyCase cases,
		    Option.map (fn e => copyExp (s, e)) dflt)
		end
	    | C.Apply(f, args, rets) =>
		C.mkApply(subst s f, subst'(s, args), subst'(s, rets))
	    | C.Throw(k, args) => C.mkThrow(subst s k, subst'(s, args))
	  (* end case *))

  (* beta-reduce a lambda application; the resulting term will have
   * fresh bound variables.  This operation also correctly preserves the
   * census counts of the parameters and arguments (but not the function
   * name itself), assuming that the original counts are correct.
   *)
    fun applyLambda (C.FB{f, params, rets=rets', body}, args, rets) = let
	  fun err msg = raise Fail(concat[
		  msg, " mismatch in application of ", CV.toString f, ":",
		  CPSTyUtil.toString(CV.typeOf f)
		])
	  val s = extend' (empty, params, args) handle _ => err "param/arg"
	  val s = extend' (s, rets', rets) handle _ => err "rets'/rets"
	  fun adjust (arg as VarRep.V{useCnt, ...}, param) = (
		CV.combineAppUseCnts (arg, param);
		useCnt := !useCnt - 1)
	  in
	    ListPair.app adjust (args, params);
	    ListPair.app adjust (rets, rets');
	    copyExp (s, body)
	  end

  (* create a copy of a BOM term with fresh bound variables *)
    fun copyLambda fb = #2 (copyOneLambda (empty, fb))

  (* create a copy of a list of mutually recursive functions *)
    fun copyLambdas fbs = #2 (copyFBs (empty, fbs))

    fun applyToBoundVars func (C.MODULE{externs, body, ...}) = let
	  fun applyToFBs fbs = (
		List.app (fn (C.FB{f, ...}) => func f) fbs;
		List.app (fn (C.FB{params, rets, body, ...}) => (
		    List.app func params;
		    List.app func rets;
		    applyToExp body)
		  ) fbs)
	  and applyToExp (C.Exp(_, e)) = (case e
		 of (C.Let(lhs, rhs, e)) => (
		      List.app func lhs;
		      applyToExp e)
		  | (C.Fun(fbs, e)) => (
		      applyToFBs fbs;
		      applyToExp e)
		  | (C.Cont(fb, e)) => (
		      applyToFBs [fb];
		      applyToExp e)
		  | (C.If(_, e1, e2)) => (applyToExp e1; applyToExp e2)
		  | (C.Switch(x, cases, dflt)) => (
		      List.app (fn (_, e) => applyToExp e) cases;
		      Option.app applyToExp dflt)
		  | (C.Apply _) => ()
		  | (C.Throw _) => ()
		(* end case *))
	  in
	    List.app (fn (CFunctions.CFun{var, ...}) => func var) externs;
	    applyToFBs [body]
	  end

  end
