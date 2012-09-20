(* dead-funs.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Dead-code elimination for groups of recursive functions that only call themselves.
 * Unlike the contraction phase, this pass can handle functions that include multiple
 * recursive calls.
 *
 * NOTE: this pass cannot be done prior to HLOp expansion and it depends on accurate
 * census info.
 *)

structure DeadFuns : sig

    val transform : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BU = BOMUtil
    structure C = Census
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntElim = ST.newCounter "dead-fns:elim"

  (* count a use *)
    fun use env f = (case BV.Map.find(env, f)
	   of SOME cnt => (cnt := !cnt + 1)
	    | NONE => ()
	  (* end case *))

    fun uses (env, xs) = List.app (use env) xs

    fun extend (B.FB{f, ...}, env) = BV.Map.insert(env, f, ref 0)

  (* a function is dead is all of its uses are recursive *)
    fun isDead env (B.FB{f, ...}) = (! (valOf (BV.Map.find (env, f))) = BV.useCount f)

    fun doFB env (B.FB{f, params, exh, body}) = B.mkLambda{
	    f = f, params = params, exh = exh,
	    body = doExp (env, body)
	  }

    and doFBs (env, fbs, scopeExp) = let
	  val env = List.foldl extend env fbs
	  val fbs = List.map (doFB env) fbs
	  in
	    if (List.all (isDead env) fbs)
	      then (
		ST.tick cntElim;
		List.app (fn (B.FB{body, ...}) => C.delete body) fbs;
		scopeExp)
	      else B.mkFun(fbs, scopeExp)
	  end

    and doExp (env, e0 as B.E_Pt(_, t)) = (case t
	   of B.E_Let(xs, e1, e2) => B.mkLet(xs, doExp(env, e1), doExp(env, e2))
	    | B.E_Stmt(xs, rhs, e) => (
		BU.appRHS (use env) rhs;  (* check rhs for references *)
		B.mkStmt(xs, rhs, doExp(env, e)))
	    | B.E_Fun(fbs, e) => doFBs (env, fbs, doExp(env, e))
	    | B.E_Cont(fb, e) => let
		val env = extend (fb, env)
		val fb = doFB env fb
		in
		  if isDead env fb
		    then (ST.tick cntElim; doExp(env, e))
		    else B.mkCont(fb, doExp(env, e))
		end
	    | B.E_If(x, e1, e2) => (
	      (* NOTE: x cannot be a function reference *)
		B.mkIf(x, doExp(env, e1), doExp(env, e2)))
	    | B.E_Case(x, cases, dflt) => let
		fun doCase (p, e) = (p, doExp(env, e))
		val dflt = Option.map (fn e => doExp(env, e)) dflt
		in
		(* NOTE: x cannot be a function reference *)
		  B.mkCase(x, List.map doCase cases, dflt)
		end
	    | B.E_Apply(f, xs, ys) => (
		uses (env, f::xs);
		uses (env, ys);
		e0)
	    | B.E_Throw(_, xs) => (uses (env, xs); e0)
	    | B.E_Ret xs => (List.app (use env) xs; e0)
	    | B.E_HLOp(_, xs, ys) => (
		uses (env, xs);
		uses (env, ys);
		e0)
	  (* end case *))

    fun transform (B.MODULE{name, externs, hlops, rewrites, body}) = let
	  val body = doFB BV.Map.empty body
	  in
	    B.MODULE{
		name = name,
		externs = externs,
		hlops = hlops,
		rewrites = rewrites,
		body = body
	      }
	  end

  end

