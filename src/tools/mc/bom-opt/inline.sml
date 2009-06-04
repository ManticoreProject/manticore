(* inline.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Expansive inlining for BOM representation.
 *
 * For recursive functions, we make a local copy of the loop-nest, but we do not
 * do any unrolling of the loop.
 *)

structure Inline : sig

    val transform : {specializeRecFuns : bool} -> BOM.module -> BOM.module

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy
    structure U = BOMUtil
    structure VSet = BV.Set
    structure P = PrimUtil
    structure C = Census
    structure ST = Stats

  (* controls *)
    val inlineFlg = ref true
    val inlineDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register BOMOptControls.registry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	      envName = NONE
	    }) [
	      Controls.control {
		  ctl = inlineFlg,
		  name = "enable-inline",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "enable expansive inlining"
		},
	      Controls.control {
		  ctl = inlineDebug,
		  name = "inline-debug",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "debug expansive inlining"
		}
	    ]


(* +DEBUG *)
    fun v2s v = BV.toString v
    fun vl2s vl = let
	  fun f [] = []
	    | f [x] = [v2s x]
	    | f (x::r) = v2s x :: ", " :: f r
	  in
	    concat(f vl)
	  end
    fun prl l = if (! inlineDebug)
	  then TextIO.output(TextIO.stdOut, String.concat("[INLINE] " :: l))
	  else ()
(* -DEBUG *)


  (********** Counters for statistics **********)
 
    val cntSimple	= ST.newCounter "inline:simple"
    val cntRec		= ST.newCounter "inline:rec"
    val cntDeadFn	= ST.newCounter "inline:dead-fun-elim"


  (********** Utility code **********)

    fun nameOfFB (B.FB{f, ...}) = f

    fun copyExp (subst, e) = let
	  val e = U.copyExp(subst, e)
	  in
	    C.initExp e;
	    e
	  end

    fun copyFBs fbs = let
	  val fbs = U.copyLambdas fbs
	  in
	    C.initLambdas fbs;
	    fbs
	  end

  (* extend the environment, inserting type casts as necessary
   *)
    fun extendWithCasts {env, fromVars, toVars} = let
        (* FIXME -- Do this right! *)
          fun needsCast (fromTy, toTy) = (case fromTy
                 of BTy.T_Any => not (BOMTyUtil.equal (BTy.T_Any, toTy)) 
                  | BTy.T_Tuple(b, ts) => (case toTy
                       of BTy.T_Tuple (b', ts') => ListPair.exists needsCast (ts, ts')
                        | _ => false
                      (* end case *))
                  | _ => false
                (* end case *))
          fun mkCasts ([], [], fromVars', casts) = (List.rev fromVars', List.rev casts)
            | mkCasts (_::_, [], _, _) = raise Fail "more fromVars than toVars"
            | mkCasts ([], _::_, _, _) = raise Fail "more toVars than fromVars"
            | mkCasts (fromVar::fromVars, toVar::toVars, fromVars', casts) = let
                val fromTy = BV.typeOf fromVar
                val toTy = BV.typeOf toVar
                in
                  if not (needsCast (fromTy, toTy))
                    then mkCasts (fromVars, toVars, fromVar::fromVars', casts)
                    else let
                      val name = let val x = BV.nameOf fromVar
                            in 
                              concat ["_cast", (if String.isPrefix "_" x then "" else "_"), x]
                            end
                      val c = BV.new (name, toTy)
                      val _ = Census.incUseCnt c (* because bind will decrement the count *)
                      val cast = ([c], B.E_Cast(toTy, fromVar))
                      in
                        mkCasts (fromVars, toVars, c::fromVars', cast::casts)
                      end
                end
          val (fromVars',casts) = mkCasts (fromVars, toVars, [], [])
          fun bind (fromVar', toVar, env) = (
                C.decUseCnt fromVar'; (* use of argument is going away! *)
                U.extend (env, toVar, fromVar'))
          val env' = ListPair.foldl bind env (fromVars', toVars)
          in
	    (env', casts)
          end

  (********** Analysis for recursive functions **********)

(* FIXME: using a list to represent the set of current functions doesn't scale! *)
    fun analyse (B.FB{body, ...}) = let
	  val {peekFn, clrFn, setFn, getFn} =
		BV.newProp (fn f => raise Fail(concat["binding(", v2s f, ")"]))
	  val {peekFn=peekRecFn, getFn=getRecCnt, setFn=setRecCnt, clrFn=clrRecCnt} =
		BV.newProp (fn _ => 0)
	  val recFns = ref []
	  fun incRecCnt f = setRecCnt(f, getRecCnt f + 1)
	  fun mark (fbs : B.lambda list, f) = (case peekFn f
		 of NONE => (
		      prl["mark ", v2s f, " recursive\n"];
		      setFn(f, fbs);
		      recFns := f :: !recFns)
		  | SOME _ => ()
		(* end case *))
	  fun clear () = List.app (fn f => (clrRecCnt f; clrFn f)) (!recFns)
	  fun analE (enclFns, curFns, B.E_Pt(_, t)) = (case t
		 of B.E_Let(_, e1, e2) => (analE(enclFns, curFns, e1); analE(enclFns, curFns, e2))
		  | B.E_Stmt(_, _, e) => analE(enclFns, curFns, e)
		  | B.E_Fun(fbs, e) => (analFBs (enclFns, fbs); analE(enclFns, curFns, e))
		  | B.E_Cont(B.FB{body, ...}, e) => (
		      analE(enclFns, curFns, body); analE(enclFns, curFns, e))
		  | B.E_If(_, e1, e2) => (analE(enclFns, curFns, e1); analE(enclFns, curFns, e2))
		  | B.E_Case(x, cases, dflt) => (
		      List.app (fn (_, e) => analE(enclFns, curFns, e)) cases;
		      case dflt of SOME e => analE(enclFns, curFns, e) | _ => ())
		  | B.E_Apply(f, _, _) =>
		      if VSet.member(enclFns, f)
			then (
			  incRecCnt f;
			(* is f bound in the innermost enclosing binding? *)
			  if List.exists (fn g => BV.same(f, nameOfFB g)) curFns
			    then mark(curFns, f)
			    else ())
			else ()
		  | B.E_Throw _ => ()
		  | B.E_Ret _ => ()
		  | B.E_HLOp _ => ()
		(* end case *))
	  and analFBs (enclFns, fbs) = let
		val enclFns = List.foldl (fn (B.FB{f, ...}, efns) => VSet.add(efns, f)) enclFns fbs
		in
		  List.app (fn (B.FB{f, body, ...}) => analE(enclFns, fbs, body)) fbs
		end
	  in
	    analE (VSet.empty, [], body);
	    { isRec = Option.isSome o peekFn, bindingOf = getFn, clearMarks = clear,
	      recAppCnt = fn x => Option.getOpt(peekRecFn x, 0)
	    }
	  end


  (********** The inlining environment **********)

    val scale = 2
    val initK = 10

  (* two-step decimation function, but we never go below a scale of 1 *)
    fun dec' k = if (k = initK) then k div scale else 1

  (* the environment used for making inlining decisions.  The factor k is
   * used scale the size of the application, while the set s contains those
   * functions that should not be inlined; either because we are inlining
   * them or we are in their body.
   *)
    datatype env = E of {k : int, s : VSet.set}

  (* add the function f to the environment *)
    fun addFun (E{k, s}, f) = E{k=k, s=VSet.add(s, f)}

  (* add the function f and decrement the size factor *)
    fun addAndDec (E{k, s}, f) = E{k=dec' k, s=VSet.add(s, f)}

  (* the initial envronment *)
    fun initEnv () = E{k = initK, s = VSet.empty}


  (********** Inlining **********)

  (* test to see if a function application ``f(args / rets)'' should be
   * inlined.  If so, return SOME(fb), where fb is the lambda
   * bound to f, otherwise return NONE.
   *)
    fun shouldInline (E{k, s}, f, args, rets) = if VSet.member(s, f)
	  then NONE
	  else (case BV.kindOf f
	     of B.VK_Fun(fb as B.FB{body, ...}) =>
		  if Sizes.smallerThan(body, k * Sizes.sizeOfApply(f, args, rets))
		    then SOME fb
		    else NONE
	      | _ => NONE
	    (* end case *))


  (* apply expansive inlining to the functions in the module *)
    fun inline (specializeRecFuns, B.MODULE{name, externs, hlops, body}) = let
	  val {isRec, bindingOf, clearMarks, recAppCnt} = analyse body
	  fun doExp (env, exp as B.E_Pt(pt, term)) = (case term
		 of B.E_Let(xs, e1, e2) => let
	              val e1 = doExp(env, e1)
		      in
			List.app (fn x => BV.setKind(x, B.VK_Let e1)) xs;
			B.mkLet(xs, e1, doExp(env, e2))
		      end
		  | B.E_Stmt(xs, rhs, e) => B.mkStmt(xs, rhs, doExp(env, e))
		  | B.E_Fun(fbs, e) => let
		    (* We apply inline expansion to the scope of the fbs
		     * first for two reasons: inline expansion may
		     * render some of the functions dead, and it is
		     * necessary to get the scaling factor right for
		     * recursive inlining.
		     *)
		      val e = doExp (env, e)
		    (* remove any dead functions from the list of function
		     * bindings.
		     *)
		      fun doFB (B.FB{f, params, exh, body}) = if (BV.useCount f > 0)
			    then SOME(B.FB{
				f=f, params=params, exh=exh,
				body=doExp(addFun(env, f), body)
			      })
			    else (
			      ST.tick cntDeadFn;
			      prl["remove dead function ", v2s f, "\n"];
			      C.delete body;
			      NONE)
		      in
			case List.mapPartial doFB fbs
			 of [] => e
			  | fbs => B.mkFun(fbs, e)
			(* end case *)
		      end
		  | B.E_Cont(fb, e) => let
		      val fb = doLambda(env, fb)
		      in
			BV.setKind(nameOfFB fb, B.VK_Cont fb);
			B.mkCont(fb, doExp(env, e))
		      end
		  | B.E_If(x, e1, e2) =>
		      B.mkIf(x, doExp(env, e1), doExp(env, e2))
		  | B.E_Case(x, cases, dflt) => let
		      fun doCase (pat, e) = (pat, doExp(env, e))
		      in
			B.mkCase(x, List.map doCase cases,
			  Option.map (fn e => doExp(env, e)) dflt)
		      end
		  | B.E_Apply(f, args, rets) => (case shouldInline(env, f, args, rets)
		       of SOME(B.FB{params, exh, body, ...}) => if isRec f
			    then if specializeRecFuns andalso (BV.appCntOf f - recAppCnt f > 1)
			      then (
(* FIXME: we probably should only specialize a recursive function nest when the total size
 * of the nest is smaller than some limit.
 *)
				ST.tick cntRec;
				inlineAppRecFn (env, f, args, rets))
			      else exp
			    else (
			      ST.tick cntSimple;
			      prl [
				  "inline application ", v2s f, "(",
				  vl2s args, " / ", vl2s rets, ")\n"
				];
			      inlineApp (env, f, rets@args, exh@params, body))
			| NONE => exp
		      (* end case *))
(* should we inline throws here? *)
		  | B.E_Throw _ => exp
		  | B.E_Ret _ => exp
		  | B.E_HLOp _ => exp
		(* end case *))
	  and doLambda (env, B.FB{f, params, exh, body}) =
		B.FB{f=f, params=params, exh=exh, body=doExp(env, body)}
	  and inlineApp (env, f, args, params, body) = let
	      (* Extend the substitution to map the parameter to the argument and decrement
	       * the argument use count.
	       *)
                val (argsForParams, casts) = extendWithCasts {env = U.empty, fromVars = args, toVars = params}
		in
		  C.decAppCnt f;
		  B.mkStmts (casts, doExp (addAndDec (env, f), copyExp (argsForParams, body)))
		end
	(* we inline recursive function groups by making a local copy of the functions *)
	  and inlineAppRecFn (env, f, args, rets) = let
		val fbs = bindingOf f
	      (* find the position of f in fbs *)
		val pos = let
		      fun find (i, []) = raise Fail("cannot find " ^ v2s f)
			| find (i, fb::r) = if BV.same(f, nameOfFB fb) then i else find(i+1, r)
		      in
			find(0, fbs)
		      end
	      (* make a fresh copy of the function bindings *)
		val fbs = copyFBs fbs
	      (* get the new name for f *)
		val f' = nameOfFB(List.nth (fbs, pos))
	      (* recursively inline expand the bodies of the fbs *)
		val fbs = List.map (fn fb => doLambda(addAndDec(env, nameOfFB fb), fb)) fbs
		in
		  prl [
		      "inline application of recursive function ", v2s f, "(",
		      vl2s args, " / ", vl2s rets, ")\n"
		    ];
		  C.decAppCnt f;
		  C.incAppCnt f';
		  B.mkFun(fbs, B.mkApply(f', args, rets))
		end
	  val body = doLambda (initEnv(), body)
	  in
	    clearMarks ();
	    B.MODULE{
		name = name,
		externs = externs,
		hlops = hlops,
		body = body
	      }
	  end

    fun transform {specializeRecFuns} m = if !inlineFlg then inline (specializeRecFuns, m) else m

  end
