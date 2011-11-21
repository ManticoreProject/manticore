(* census.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Census for CPS variables.
 *)

structure CPSCensus : sig

    val census : CPS.module -> unit

  (* update variable counts *)
    val clear : CPS.var -> unit		(* clear both use and application counts *)
    val incUseCnt : CPS.var -> unit	(* increment use count *)
    val incAppCnt : CPS.var -> unit	(* increment both use and application counts *)
    val decUseCnt : CPS.var -> unit	(* decrement use count *)
    val decAppCnt : CPS.var -> unit	(* decrement both use and application counts *)

  (* adjust counts to account for the deletion of an expression *)
    val delete : (CPS.var CPS.Var.Map.map * CPS.exp) -> unit

  end = struct

    structure C = CPS

    fun clear x = (C.Var.clrCount x; C.Var.appCntRmv x)
    fun inc x = C.Var.addToCount(x, 1)
    fun dec x = C.Var.addToCount(x, ~1)

  (* record an application use *)
    fun appUse x = let
	  val appCnt = C.Var.appCntRef x
	  in
	    inc x;
	    appCnt := !appCnt + 1
	  end

    fun clearFB (C.FB{f, params, rets, ...}) = (
	  clear f;
	  List.app clear params;
	  List.app clear rets)

    fun census (C.MODULE{body, externs, ...}) = let
	  fun doExp (C.Exp(_, t)) = (case t
		 of (C.Let(lhs, rhs, e)) => (
		      List.app clear lhs;
		      CPSUtil.appRHS inc rhs;
		      doExp e)
		  | (C.Fun(fbs, e)) => (
		      List.app clearFB fbs;
		      List.app doFB fbs;
		      doExp e)
		  | (C.Cont(fb, e)) => (
		      clearFB fb;
		      doFB fb;
		      doExp e)
		  | (C.If(cond, e1, e2)) => (CondUtil.app inc cond; doExp e1; doExp e2)
		  | (C.Switch(x, cases, dflt)) => (
		      inc x;
		      List.app (fn (_, e) => doExp e) cases;
		      Option.app doExp dflt)
		  | (C.Apply(f, args, rets)) => (
		      appUse f;
		      List.app inc args;
		      List.app inc rets)
		  | (C.Throw(k, args)) => (
		      appUse k;
		      List.app inc args)
		(* end case *))
	  and doFB (C.FB{body, ...}) = doExp body
	  fun clrCFun cf = clear(CFunctions.varOf cf)
	  in
	    List.app clrCFun externs;
	    clearFB body;
	    doFB body
	  end

    val incUseCnt = inc
    val incAppCnt = appUse
    fun decUseCnt x = C.Var.addToCount(x, ~1)
    fun decAppCnt x = let
	  val appCnt = C.Var.appCntRef x
	  in
	    dec x;
	    appCnt := !appCnt - 1
	  end

  (* adjust counts to account for the deletion of an expression *)
    fun delete (env, e) = let
	  fun dec x = (case CPS.Var.Map.find(env, x)
		 of SOME y => decUseCnt y
		  | NONE => decUseCnt x
		(* end case *))
	  fun decApp x = (case CPS.Var.Map.find(env, x)
		 of SOME y => decAppCnt y
		  | NONE => decAppCnt x
		(* end case *))
	  fun doExp (C.Exp(_, t)) = (case t
		 of (C.Let(lhs, rhs, e)) => (
		      CPSUtil.appRHS dec rhs;
		      doExp e)
		  | (C.Fun(fbs, e)) => (
		      List.app doFB fbs;
		      doExp e)
		  | (C.Cont(fb, e)) => (
		      doFB fb;
		      doExp e)
		  | (C.If(cond, e1, e2)) => (CondUtil.app dec cond; doExp e1; doExp e2)
		  | (C.Switch(x, cases, dflt)) => (
		      dec x;
		      List.app (fn (_, e) => doExp e) cases;
		      Option.app doExp dflt)
		  | (C.Apply(f, args, rets)) => (
		      decApp f;
		      List.app dec args;
		      List.app dec rets)
		  | (C.Throw(k, args)) => (
		      decApp k;
		      List.app dec args)
		(* end case *))
	  and doFB (C.FB{body, ...}) = doExp body
	  in
	    doExp e
	  end

  end
