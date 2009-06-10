(* contract.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Contraction for the CPS representation.
 *)

structure Contract : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntUnusedStmt           = ST.newCounter "cps-contract:unused-stmt"
    val cntVarRename		= ST.newCounter "cps-contract:var-rename"
    val cntSelectConst          = ST.newCounter "cps-contract:select-const"
    val cntIfConst              = ST.newCounter "cps-contract:if-const"
    val cntBeta                 = ST.newCounter "cps-contract:beta"
    val cntBetaCont             = ST.newCounter "cps-contract:beta-cont"
    val cntUnusedFun		= ST.newCounter "cps-contract:unused-fun"
    val cntUnusedCont		= ST.newCounter "cps-contract:unused-cont"
    val cntUnusedCFun		= ST.newCounter "cps-contract:unused-cfun"
  (* first and last counters *)
    val firstCounter            = cntUnusedStmt
    val lastCounter             = cntUnusedCFun
  (* these counters track the number of contraction phases/iterations *)
    val cntPhases		= ST.newCounter "cfg-contract:phases"
    val cntIters		= ST.newCounter "cfg-contract:iterations"

  (********** Get variable info **********)
    fun bindingOf (VarRep.V{kind, ...}) = !kind
    fun setBinding (VarRep.V{kind, ...}, b) = kind := b
    fun setBindings ([x], b) = setBinding(x, b)
      | setBindings _ = ()
    fun useCntRef (VarRep.V{useCnt, ...}) = useCnt
    fun useCntOf v = !(useCntRef v)
    val appCntRef = CV.appCntRef
    val appCntOf = CV.appCntOf
    val combineAppUseCnts = CV.combineAppUseCnts

  (* functions to update census counts *)
    fun inc x = BV.addToCount(x, 1)
    fun dec x = BV.addToCount(x, ~1)
    val dec' = List.app dec
    fun unused x = (useCntOf x = 0)

  (* Variable renaming *)
    fun rename (env, x, y) = (
	(* every use of x will be replaced by a use of y *)
	  combineAppUseCnts(y, x);
	  VMap.insert(env, x, y))

    fun rename' (env, [], []) = env
      | rename' (env, x::xs, y::ys) = rename' (VMap.insert(env, x, y), xs, ys)
      | rename' _ = raise Fail "rename': arity mismatch"

    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))

    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst(env, x) :: subst'(env, xs)

    fun substDec (env, x) = (case VMap.find(env, x)
	   of SOME y => dec y
	    | NONE => dec x
	  (* end case *))

    fun substDec' (env, xs) = List.map (fn x => substDec(env, x)) xs


    fun doExp (env, exp as C.Exp(_, e)) = (case e
	   of C.Let(lhs, C.Var rhs, e) => (
		ST.tick cntVarRename;
		dec' rhs;
		doExp (reame'(env, lhs, rhs), e))
	    | C.Let([y], rhs as C.Select(i, x), e) => if unused y
		then (
		  ST.tick cntUnusedStmt;
		  substDec (env, x);
		  doExp (env, e))
		else let
		  val x = subst(env, x)
		  in
		    case bindingOf x
		     of C.VK_Let(C.Alloc(CTy.T_Tuple(false, _), xs)) => let
			  val x' = List.nth(xs, i)
			  val env = rename(env, x, x')
			  in
			    ST.tick cntSelectConst;
			    dec x;
			    doExp (env, e)
			  end
		      | _ => let
			  val e = doExp (env, e)
			  in
			    if unused y
			      then (
				ST.tick cntUnusedStmt;
				dec x;
				e)
			      else C.mkLet([y], rhs, e)
			  end
		    (* end case *)
		  end
	    | C.Let([], C.Update(i, x, z), e) =>
		C.mkLet([], C.Update(i, subst(env, x), subst(env, z)),
		  doExp(env, e))
	    | C.Let(lhs, C.CCall(cf, xs), e) =>
(* FIXME: check if cf is bound to a pure C function *)
		C.mkLet(lhs, C.CCall(cf, subst'(env, xs)), doExp(env, e))
	    | C.Let([], C.VPStore(n, x, z), e) =>
		C.mkLet([], C.VPStore(n, subst(env, x), subst(env, z)),
		  doExp(env, e))
	    | C.Let(lhs, rhs, e) => if List.all unused lhs
		then (
		  ST.tick cntUnusedStmt;
		  CPSUtil.appRHS (fn x => substDec (env, x)) rhs;
		  doExp (env, e))
		else let
		  val rhs = CPSUtil.mapRHS (fn x => subst(env, x)) rhs
		  val _ = setBindings (lhs, C.VK_Let rhs)
		  val e = doExp (env, e)
		  in
		    if List.all unused lhs
		      then (
			ST.tick cntUnusedStmt;
			CPSUtil.appRHS dec rhs;
			e)
		      else C.mkLet(lhs, rhs, e)
		  end
	    | C.Fun(fbs, e) => let
	      (* blackhole to avoid recursive inlining *)
		val _ = List.app
		      (fn (C.FB{f, ...}) => setBinding(f, C.VK_Cont fb))
			fbs
		fun doFB (C.FB{f, params, rets, body}) = if unused f
		      then (
			ST.tick cntUnusedFun;
			Census.delete body;
			NONE)
		      else SOME(C.mkFB{
			  f=f, params=params, rets=rets,
			  body=doExp(env, body)
			})
		val fbs = List.mapPartial doFB fbs
		val _ = List.app
		      (fn (fb as C.FB{f, ...}) => setBinding(f, C.VK_Fun fb))
			fbs
		val e = doExp(env, e)
		fun filterDead (fb as C.FB{f, body, ...}) = if unused f
		      then (
			ST.tick cntUnusedFun;
			Census.delete body;
			NONE)
		      else SOME fb
		in
		  case List.mapPartial filterDead fbs
		   of [] => e
		    | fbs => C.mkFun(fbs, e)
		  (* end case *)
		end
	    | C.Cont(C.FB{f, params, rets, body}, e) => if unused f
		then (
		  ST.tick cntUnusedCont;
		  Census.delete body;
		  doExp (env, e))
		else let
		(* blackhole to avoid recursive inlining *)
		  val _ = setBinding(f, C.VK_None)
		  val body = doExp(env, body)
		  val fb = C.mkFB{f=f, params=params, rets=rets, body=body}
		  val _ = setBinding(f, C.VK_Cont fb)
		  val e = doExp (env, e)
		  in
		    if unused f
		      then (
			ST.tick cntUnusedCont; 
			Census.delete body;
			doExp (env, e))
		      else C.mkCont(fb, e)
		  end
	    | C.If(x, e1, e2) => let
		val x = subst(env, x)
		in
		  case bindingOf x
		   of C.VK_Let(C.Const(_, lit)) => (
			ST.tick cntIfConst;
			if (Literal.same(lit, Literal.trueLit))
			  then (Census.delete(env, e2); doExp(env, e1))
			  else (Census.delete(env, e1); doExp(env, e2)))
		    | _ => C.mkIf(x, doExp(env, e1), doExp(env, e2))
		  (* end case *)
		end
	    | C.Switch(x, cases, dflt) => let
		val x = subst(env, x)
		in
		end
	    | C.Apply(f, args, conts) => let
		val f = subst(env, f)
		val args = subst'(env, args)
		val conts = subst'(env, conts)
		in
		  case bindingOf f
		   of C.VK_Fun(C.FB{params, rets, body, ...}) =>
			if useCntOf f = 1
			  then ((* inline function that is only called once *)
			    ST.tick cntBeta;
			    inline (env, params@rets, body, args@conts))
			  else C.mkApply(f, args, conts)
		    | _ => C.mkApply(f, args, conts)
		  (* end case *)
		end
	    | C.Throw(k, args) => let
		val k = subst(env, k)
		val args = subst'(env, args)
		in
		  case bindingOf k
		   of C.VK_Cont(C.FB{params, body, ...}) =>
			if useCntOf k = 1
			  then ((* inline continuation that is only called once *)
			    ST.tick cntBetaCont;
			    inline (env, params, body, args))
			  else C.mkThrow(k, args)
		    | _ => C.mkThrow(k, args)
		  (* end case *)
		end
	  (* end case *))

    and inline (env, params, body, args) = let
	  val env = rename' (env, params, args)
	  in
	    List.app dec args;
	    doExp (env, body)
	  end

    fun transform (C.MODULE{name, externs, body}) = let
	  val C.FB{f, params, rets, body} = body
	(* iterate contraction until we reach a fixed point *)
	  fun ticks () = ST.sum {from = firstCounter, to = lastCounter}
	  fun loop (body, prevSum) = let
		val _ = ST.tick cntIters
		val body = List.mapPartial contractFunc body
		val sum = ticks()
		in
		  if (prevSum <> sum)
		    then loop (body, sum)
		    else body
		end
	  val body = loop (body, ticks())
	  val fb = C.mkFB{f=f, params=params, rets=rets, body=body}
	  in
	    C.MODULE{name=name, externs=externs, body=body}
	  end

  end
