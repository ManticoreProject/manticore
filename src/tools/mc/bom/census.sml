(* census.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Initialize the use counts of the variables in a BOM module.
 *)

structure Census : sig

    val census : BOM.module -> unit

    val initLambda  : BOM.lambda -> unit
    val initLambdas : BOM.lambda list -> unit
    val initExp     : BOM.exp -> unit

  (* For each variable use (and application use) in the expression, decrement
   * the (renamed) variable's count by one.
   *)
    val delete : BOM.exp -> unit
    val deleteWithRenaming : (BOMUtil.subst * BOM.exp) -> unit

  (* update variable counts *)
    val clear : BOM.var -> unit		(* clear both use and application counts *)
    val incUseCnt : BOM.var -> unit	(* increment use count *)
    val incAppCnt : BOM.var -> unit	(* increment both use and application counts *)
    val decUseCnt : BOM.var -> unit	(* decrement use count *)
    val decAppCnt : BOM.var -> unit	(* decrement both use and application counts *)

  end = struct

    structure B = BOM
    structure U = BOMUtil

    fun clear x = (B.Var.clrCount x; B.Var.appCntRmv x)
    fun inc x = B.Var.addToCount(x, 1)
    fun dec x = B.Var.addToCount(x, ~1)

  (* record an application use *)
    fun appUse x = let
	  val appCnt = B.Var.appCntRef x
	  in
	    inc x;
	    appCnt := !appCnt + 1
	  end

    fun doE (B.E_Pt(_, t)) = (case t
	   of B.E_Let(lhs, e1, e2) => (List.app clear lhs; doE e1; doE e2)
	    | B.E_Stmt(lhs, rhs, e) => (List.app clear lhs; BOMUtil.appRHS inc rhs; doE e)
	    | B.E_Fun(fbs, e) => (List.app clrFB fbs; List.app doFB fbs; doE e)
	    | B.E_Cont(fb, e) => (clrFB fb; doFB fb; doE e)
	    | B.E_If(x, e1, e2) => (inc x; doE e1; doE e2)
	    | B.E_Case(x, cases, dflt) => let
		fun doCase (B.P_DCon(_, args), e) = (List.app clear args; doE e)
		  | doCase (_, e) = doE e
		in
		  inc x;
		  List.app doCase cases;
		  Option.app doE dflt
		end
	    | B.E_Apply(f, xs, ys) => (appUse f; List.app inc xs; List.app inc ys)
	    | B.E_Throw(k, xs) => (appUse k; List.app inc xs)
	    | B.E_Ret xs => List.app inc xs
	    | B.E_HLOp(hlop, xs, ys) => 
	      (List.app inc xs; List.app inc ys)
	  (* end case *))

    and clrFB (B.FB{f, params, exh, ...}) = (
	  clear f;
	  List.app clear params; List.app clear exh)

    and doFB (B.FB{body, ...}) = doE body

    and funBind (B.FB{f, ...}) = f

    fun census (B.MODULE{externs, hlops, body, ...}) = let
	  fun clrCFun cf = clear(CFunctions.varOf cf)
	  in
	    List.app clrCFun externs;
	    B.Var.Set.app clear hlops;
	    clrFB body;
	  (* compute counts *)
	    doFB body;
	  (* We bump the count of HLOps by one to avoid prematurely deleting them *)
	    B.Var.Set.app inc hlops
	  end

    fun initLambda fb = (clrFB fb; doFB fb)

    fun initLambdas fbs = (List.app clrFB fbs; List.app doFB fbs)

    fun initExp e = doE e

    fun delete e = let
	  fun dec x = B.Var.addToCount(x, ~1)
	  fun decApp f = let
		val appCnt = B.Var.appCntRef f
		in
		  dec f;
		  appCnt := !appCnt - 1
		end
	  fun dec' xs = List.app dec xs
	  fun del (B.E_Pt(_, t)) = (case t
		 of B.E_Let(_, e1, e2) => (del e1; del e2)
		  | B.E_Stmt(_, rhs, e) => (BOMUtil.appRHS dec rhs; del e)
		  | B.E_Fun(fbs, e) => (List.app delFB fbs; del e)
		  | B.E_Cont(fb, e) => (delFB fb; del e)
		  | B.E_If(x, e1, e2) => (dec x; del e1; del e2)
		  | B.E_Case(x, cases, dflt) => (
		      dec x;
		      List.app (fn (_, e) => del e) cases;
		      Option.app del dflt)
		  | B.E_Apply(f, args, rets) => (decApp f; dec' args; dec' rets)
		  | B.E_Throw(k, args) => (decApp k; dec' args)
		  | B.E_Ret xs => dec' xs
		  | B.E_HLOp(_, args, rets) => (dec' args; dec' rets)
		(* end case *))
	  and delFB (B.FB{body, ...}) = del body
	  in
	    del e
	  end

    fun deleteWithRenaming (env, e) = let
	  val subst = U.subst env
	  fun dec x = B.Var.addToCount(subst x, ~1)
	  fun decApp f = let
		val f = subst f
		val appCnt = B.Var.appCntRef f
		in
		  B.Var.addToCount(f, ~1);
		  appCnt := !appCnt - 1
		end
	  fun dec' xs = List.app dec xs
	  fun del (B.E_Pt(_, t)) = (case t
		 of B.E_Let(_, e1, e2) => (del e1; del e2)
		  | B.E_Stmt(_, rhs, e) => (BOMUtil.appRHS dec rhs; del e)
		  | B.E_Fun(fbs, e) => (List.app delFB fbs; del e)
		  | B.E_Cont(fb, e) => (delFB fb; del e)
		  | B.E_If(x, e1, e2) => (dec x; del e1; del e2)
		  | B.E_Case(x, cases, dflt) => (
		      dec x;
		      List.app (fn (_, e) => del e) cases;
		      Option.app del dflt)
		  | B.E_Apply(f, args, rets) => (decApp f; dec' args; dec' rets)
		  | B.E_Throw(k, args) => (decApp k; dec' args)
		  | B.E_Ret xs => dec' xs
		  | B.E_HLOp(_, args, rets) => (dec' args; dec' rets)
		(* end case *))
	  and delFB (B.FB{body, ...}) = del body
	  in
	    del e
	  end

    val incUseCnt = inc
    val incAppCnt = appUse
    fun decUseCnt x = B.Var.addToCount(x, ~1)
    fun decAppCnt x = let
	  val appCnt = B.Var.appCntRef x
	  in
	    dec x;
	    appCnt := !appCnt - 1
	  end

  end
