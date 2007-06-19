(* contract.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * An Jim-Appel-style contraction phase for the BOM representation.
 * The contraction optimizations include:
 *
 *	- application of functions that are called exactly once
 *	- elimination of unused variables that are bound to "pure" expressions.
 *)

structure Contract : sig

    val optimize : BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure C = Census
    structure U = BOMUtil
    structure ST = Stats

  (********** Counters for statistics **********)
    val cntUnusedStmt		= ST.newCounter "contract:unused-stmt"
    val cntLetRename		= ST.newCounter "contract:let-rename"
    val cntLetElim		= ST.newCounter "contract:let-elim"
    val cntLetFloat		= ST.newCounter "contract:let-float"
    val cntUnusedCast		= ST.newCounter "contract:unused-cast"
    val cntIdCast		= ST.newCounter "contract:identity-cast"
    val cntUnusedSelect		= ST.newCounter "contract:unused-select"
    val cntSelectConst		= ST.newCounter "contract:select-const"
    val cntUnusedAlloc		= ST.newCounter "contract:unused-alloc"
    val cntUnusedWrap		= ST.newCounter "contract:unused-wrap"
    val cntWrapUnwrap		= ST.newCounter "contract:wrap-unwrap"
    val cntUnusedUnwrap		= ST.newCounter "contract:unused-unwrap"
    val cntUnwrapWrap		= ST.newCounter "contract:unwrap-wrap"
    val cntUnusedPrim		= ST.newCounter "contract:unused-prim"
    val cntConstFold		= ST.newCounter "contract:const-fold"
    val cntUnusedApplyCont	= ST.newCounter "contract:unused-throw"
    val cntUnusedCFunCall	= ST.newCounter "contract:unused-cfun-call"
    val cntDeadFun		= ST.newCounter "contract:dead-fun"
    val cntDeadRecFun		= ST.newCounter "contract:dead-rec-fun"
    val cntDeadCont		= ST.newCounter "contract:dead-cont"
    val cntEta			= ST.newCounter "contract:eta"
    val cntIfNot		= ST.newCounter "contract:if-not"
    val cntIfConst		= ST.newCounter "contract:if-const"
    val cntIfReduce		= ST.newCounter "contract:if-reduce"
    val cntSwitchConst		= ST.newCounter "contract:switch-const"
    val cntBeta			= ST.newCounter "contract:beta"
    val cntBetaCont		= ST.newCounter "contract:beta-cont"
    val firstCounter		= cntUnusedStmt
    val lastCounter		= cntBetaCont
  (* these counters track the number of contraction phases/iterations *)
    val cntPhases		= ST.newCounter "contract:phases"
    val cntIters		= ST.newCounter "contract:iterations"


  (********** Get variable info **********)
    fun bindingOf (BV.V{kind, ...}) = !kind
    fun setBinding (BV.V{kind, ...}, b) = kind := b
    fun setBindings ([x], b) = setBinding(x, b)
      | setBindings _ = ()
    fun useCntRef (BV.V{useCnt, ...}) = useCnt
    fun useCntOf v = !(useCntRef v)
    val appCntRef = BV.appCntRef
    val appCntOf = BV.appCntOf

  (* functions to update census counts *)
    fun dec x = BV.addToCount(x, ~1)
    val dec' = List.app dec
    fun unused x = (useCountOf x = 0)
    fun combineAppUseCnts (arg, param) = let
	  val argU = useCntRef arg
	  in
	    argU := !argU + useCntOf param
(* FIXME: adjust app counts too *)
	  end

    fun decS (env, x) = dec(U.subst env x)
    fun decS' (env, xs) = dec'(U.subst'(env, xs))

  (* support for recording that a function has been inlined.  Note that we
   * need to distinguish between inlined and dead functions (even though
   * both have zero use counts), since when a function is inlined its body
   * has been copied, but when a function is dead, the variables it references
   * must have their counts decreased.
   *)
    fun markInlined (B.V{binding, ...}) = binding := B.VB_Inlined
    fun isInlined (B.V{binding = ref B.VB_Inlined, ...}) = true
      | isInlined _ = false

    fun += (r, i : int) = r := !r + i
    fun -= (r, i : int) = r := !r - i
    infix += -=


  (********** Variable utilities **********)
    fun vbind (env, v) = bindingOf(S.subst(env, v))

    fun ebind (env, v) = (case vbind(env, v)
	   of (B.VB_Bind rhs) => SOME rhs
	    | _ => NONE
	  (* end case *))

    fun sameVars ([], []) = true
      | sameVars (v1::r1, v2::r2) = BV.same(v1, v2) andalso sameVars(r1, r2)
      | sameVars _ = false


  (********** Continuation IDs **********)
    val {clrFn=clrKID, getFn=getKID, setFn=setKID, ...} = BV.newProp (fn _ => ~1)


  (********** effect analysis **********)
    fun pureRHS (B.E_Update _) = false
      | pureRHS (B.E_Prim p) = PrimUtil.isPure p
      | pureRHS (B.E_CCall of (var * var list)		(* foreign-function calls *)
      | pureRHS (B.E_VPStore _) = false
      | pureRHS _ = true

    local
      val {peekFn, setFn, ...} = ProgPt.newProp (fn _ => false)
    in
    fun pureExp (B.E_PPt(ppt, e)) = (case peekFn ppt
	   of SOME isPure => isPure
	    | NONE => let
		val isPure = (case e
		       of B.E_Let(_, e1, e2) => pureExp e1 andalso pureExp e2
			| B.E_Stmt(_, rhs, e) => pureRHS rhs andalso pureExp e
			| B.E_Fun(_, e) => pureExp e
			| B.E_Cont(_, e) => pureExp e
			| B.E_If(_, e1, e2) => pureExp e1 andalso pureExp e2
			| B.E_Case(x, cases, dflt) =>
			    List.all (fn (_, e) => pureExp e) cases
			    andalso (case dflt of SOME e => pureExp e | _ => true)
			| B.E_Apply(f, args, rets) => false
			| B.E_Throw(k, args) => false
			| B.E_Ret xs => true
			| B.E_HLOp(hlop, _, _) => HLOp.isPure hlop
		      (* end case *))
		in
		  setFn(ppt, isPure);
		  isPure
		end
    end (* local *)


  (********** Contraction **********)

  (* we use this global to hold the eta flag that the contract function gets
   * as an argument.  It isn't reentrant, but a lot easier!
   *)
    val doEta = ref false

  (* try to eta contract a function definition *)
    fun etaContract (B.FB{f, params, exh, body}) =
	  if !doEta
	    then (case body
	       of (B.E_Pt(_, B.E_Apply(g, args, rets))) => let
		    fun eq ([], []) = true
		      | eq (x::xs, y::ys) = BV.same(x, y) andalso eq(xs, ys)
		      | eq _ = false
		    in
		      if not(BV.same(f, g))
		      andalso eq(params, args)
		      andalso eq(exh, rets)
			then SOME g
			else NONE
		    end
		| _ => NONE
	      (* end case *))
	    else NONE

    fun tryContract (xUseCnt, y, cntr) () =
	  if (!xUseCnt = 0)
	    then (
	      ST.tick cntr;
	      useCntRef y -= 1;
	      true)
	    else false

    fun tryContract' (xUseCnt, ys, cntr) () =
	  if (!xUseCnt = 0)
	    then (
	      ST.tick cntr;
	      List.app (fn y => useCntRef y -= 1) ys;
	      true)
	    else false

    datatype 
  (* contract a pure RHS form; we assume that x is used and that the variables in
   * the RHS have already been renamed.
   *)
    fun doPureRHS (x, rhs) = (case rhs
	   of B.E_Stmt(lhs, B.E_Const _) =>
	    | B.E_Stmt([x], B.E_Cast(ty, y)) =>
	    | B.E_Stmt([x], B.E_Select(i, y)) => let
		val y = subst env y
		in
		 case bindingOf(U.subst env y)
		  of B.VK_RHS(B.E_Alloc(_, ys)) => let
			val z = List.nth(ys, i)
			in
			  ST.tick cntSelectConst;

			  (U.extend(env, x, z), [])
		   | _ => (env, ([x], [B.E_Select(i, y)]))
		  (* end case *)
		end
	    | B.E_Stmt([x], B.E_AddrOf(i, y)) =>
	    | B.E_Stmt([x], B.E_Alloc(ty, ys)) =>
	    | B.E_Stmt([x], B.E_Prim p) =>
	    | B.E_Stmt([x], B.E_DCon(dc, ys)) =>
	    | B.E_Stmt([x], B.E_HostVProc) =>
	    | B.E_Stmt([x], B.E_VPLoad(n, y)) =>
	    | _ => raise Fail "impure rhs"
	  (* end case *))

    fun doExp (env, B.E_Pt(_, t), kid) = let
	  val subst = subst env
	  in
	    case t
	     of B.E_Let(lhs, rhs, e) =>
		  if List.all unused lhs andalso pureExp e
		    then (
		      ST.tick cntLetElim;
		      deleteExp(env, rhs);
		      doExp (env, e))
		    else (case doExp(env, rhs)
		       of B.E_Pt(_, B.E_Ret ys) =>
			| B.E_Pt(_, B.E_Let(xs, e1, e2)) =>
			| rhs => B.mkLet(lhs, doExp(env, rhs), doExp(env, e))
		      (* end case *))
	    (* impure statement forms *)
	      | B.E_Stmt([], B.E_Update(i, y, z)) =>
		  B.mkStmt([], B.E_Update(n, subst env y, subst env z), doExp(env, e, kid))
	      | B.E_Stmt(lhs, B.E_CCall(cf, ys)) =>
	      | B.E_Stmt([], B.E_VPStore(n, y, z), e) =>
		  B.mkStmt([], B.E_VPStore(n, subst env y, subst env z), doExp(env, e, kid))
	    (* pure statement forms; note that all pure forms produce a single
	     * result.
	     *)
	      | B.E_Stmt([x], rhs, e) => let
		  val rhs = U.substRHS(env, rhs)
		  fun tryContract () = if unused x
			then (
			  ST.tick cntUnusedStmt;
			  U.appRHS dec rhs;
			  true)
			else false
		  in
		    if tryContract()
		      then doExp(env, e, kid)
		      else (case doPureRHS(x, rhs)
			 of 
	      | B.E_Stmt _ => raise Fail "bogus E_Stmt"
	      | B.E_Fun([fb], e) => let
		    fun deadFun () = (
			  ST.tick cntDeadFun;
			  C.delete (env, body))
		  (* reduce the function body and its scope *)
		    fun reduceRest () = let
			  val e' = doExp (e, env, kid)
			  in
			    if (isInlined f)
			      then e'
			    else if (useCntOf f = 0)
			      then (deadFun(); e')
			      else let
				val lambda' = doFunBody(lambda, env, kid)
				in
				  case etaContract lambda'
				   of NONE => C.mkFun([lambda'], e')
				    | (SOME g) => (
					ST.tick cntEta;
				      (* adjust counts of g *)
					useCntRef g += (useCntOf f - 1);
					appCntRef g += (appCntOf f - 1);
				      (* replace f with g in e' *)
					S.applyToExp(S.singleton(f, g), e'))
				  (* end case *)
				end
			    end
		    in
		      case (useCntOf f)
		       of 0 => (deadFun(); doExp (e, env, kid))
			| 1 => if (appCntOf f = 1)
		            then let
			      val e' = doExp (e, env, kid)
			      in
				if not(isInlined f)
				  then (
				    ST.tick cntDeadRecFun;
				    C.delete (env, body))
				  else ();
				e'
			      end
		            else reduceRest()
			| _ => reduceRest()
		      (* end case *)
		    end
	      | B.E_Fun(fbs, e) => let
		  (* check to see if a function is dead and do the bookkeeping
		   * if it is.
		   *)
		    fun deadFun (lambda as (f, _, body)) =
			  if (useCntOf f = 0)
			    then (
			      ST.tick cntDeadFun;
			      C.delete (env, body);
			      NONE)
			    else SOME lambda
		  (* check to see if a function has been inlined or is dead *)
		    fun deadFun' (lambda as (f, _, _)) =
			  if (isInlined f)
			    then NONE
			    else deadFun lambda
		  (* process a function body, but skip those that are going to
		   * be eliminated (i.e., have zero use counts).
		   *)
		    fun doFB lambda = if (useCntOf(#1 lambda) = 0)
			  then lambda
			  else doFunBody (lambda, env, kid)
		    in
		      case List.mapPartial deadFun fbs
		       of [] => doExp (e, env, kid)
			| fbs => let
			    val e' = doExp(e, env, kid)
			    val fbs = List.mapPartial deadFun' fbs
			    val fbs = List.map doFB fbs
			    in
  (** NOTE: this code needs to be modified to also support etaContraction, but
   ** I'm not sure how to handle the renaming (perhaps as a second pass?).
   ** [jhr; 2000-05-02]
   **)
			      case List.mapPartial deadFun' fbs
			       of [] => e'
				| fbs => C.mkFun(fbs, e')
			      (* end case *)
			    end
		      (* end case *)
		    end
	      | B.E_Cont(fb, e) => let
		    fun isDead () = if (useCntOf k = 0)
			  then (
			    ST.tick cntDeadCont;
			    C.delete (env, body);
			    true)
			  else false
		    in
		      if isDead()
			then doExp(e, env, kid)
			else let
			  val body' = doExp(body, env, kid)
			(* we record the kid as a property of k, so that we
			 * know when it is correct to inline a throw to k
			 * in the expression e.
			 *)
			  val _ = setKID(k, kid)
			  val e' = doExp(e, env, kid)
			  in
			    clrKID k;  (* clear KID property *)
			    if isDead()
			      then e'
			      else C.mkCont((k, params, body'), e')
			  end
		    end
	      | B.E_If(x, e1, e2) => let
		  val x = U.subst subst x
		  in
		    case bindingOf x
		     of B.VK_RHS(B.E_Const(Lit.Enum b)) => (
			  ST.tick cntIfConst;
			  dec x;
			  if (b <> 0w0)
			    then (C.delete e2; doExp(subst, e1, kid))
			    else (C.delete e1; doExp(subst, e2, kid))
		      | B.VK_RHS(B.E_Prim(P.BNot y)) => (
			  ST.tick cntIfNot;
			  dec x;
			  inc y;
			  B.mkIf(y, doExp(subst, e2, kid), doExp(subst, e1, kid)))
		      | _ => B.mkIf(x, doExp(subst, e1, kid), doExp(subst, e2, kid))
		    (* end case *)
		  end
	      | B.E_Case(x, cases, dflt) => let
		  val x = U.subst subst x
		  fun doCase (pat, e) = (pat, doExp(env, e, kid))
		  in
  (* FIXME: check for the case where x is bound to a known value *)
		    B.mkCase(x,
		      List.map doCase cases,
		      Option.map (fn e => doExp(env, e, kid)) dflt)
		  end
	      | B.E_Apply(f, args, rets) => let
		  val f = U.subst subst f
		  val args = U.subst' (subst, args)
		  val rets = U.subst' (subst, rets)
		  in
		    case bindingOf f
		     of B.VK_Lambda(B.FB{params, exh, body, ...}) =>
		          if (useCountOf f = 1)
			    then ( (* beta-reduce function with single call site *)
			      markInlined f;
			      ST.tick cntBeta;
			      appCntRef f -= 1;
			      useCntRef f -= 1;
			      inlineApply {
				  env = env, kid = kid,
				  args = rets@args, params = exh@params,
				  body = body
				})
			    else B,mkApply(f, args, rets)
		      | _ => B.mkApply(f, args, rets)
		    (* end case *)
		  end
	      | B.E_Throw(k, args) => let
		  val k = U.subst subst k
		  val args = U.subst' (subst, args)
		  in
		    case bindingOf f
		     of B.VK_Lambda(B.FB{params, exh, body, ...}) =>
		          if (useCountOf k = 1) andalso (kid = getKID k)
			    then ( (* beta-reduce function with single call site *)
			      markInlined k;
			      ST.tick cntBetaCont;
			      appCntRef k -= 1;
			      useCntRef k -= 1;
			      inlineApply {
				  env = env, kid = kid,
				  args = args, params = params,
				  body = body
				})
			    else B.mkThrow(k, args)
		      | _ => B.mkThrow(k, args)
		    (* end case *)
		  end
	      | B.E_Ret xs => B.mkRet(U.subst'(subst, xs))
	      | B.E_HLOp(hlop, args, rets) =>
		  B.mkHLOp(hlop, U.subst'(subst, args), U.subst'(subst, rets))
	    (* end case *)
	  end

  (* contract the body of a function.  Prior to doing so, we null out the
   * function variable's binding so that we avoid infinite unwinding.
   *)
    and doFunBody (lambda as B.FB{f, params, exh, body}, env, kid) = let
	  val body' = (
		setBinding (f, B.VK_None);
		doExp (body, env, kid+1))
	  val lambda' = B.FB{f=f, params=params, exh=exh, body=body'}
	  in
	    setBinding (f, B.VK_Fun lambda');
	    lambda'
	  end

    and inlineApply {env, kid, args, params, body} = let
	  val env = U.extend' (subst, params, args)
	  fun adjust (arg, param) = (
		combineAppUseCnts (arg, param);
		useCntRef arg -= 1)
	  in
	    ListPair.app adjust (args, param);
	    doExp (env, body, kid)
	  end

  end
