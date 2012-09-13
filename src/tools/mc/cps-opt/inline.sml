(* inline.sml
 *
 * Performs inlining of higher-order functions, including those with
 * free variables, using reflow analysis to verify environment
 * consonance.
 *
 * BUGS:
 *
 * Skipping mutually recursive function inlining for now, unlike
 * the BOM version, which handles them.
 *
 * We do not properly track the reduction in app counts for the variable
 * that was previously the target of the original Apply/Throw.
 *)

structure Inline : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CTy = CPSTy
    structure CV = C.Var
    structure VSet = CV.Set
    structure VMap = CV.Map
    structure U = CPSUtil
    structure ST = Stats
    structure CFA = CFACPS
    structure Census = CPSCensus

  (* controls *)
    val inlineFlg = ref true
    val inlineDebug = ref false
    val inlineHOFlg = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
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
		  ctl = inlineHOFlg,
		  name = "enable-ho-inline",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "enable higher-order extension to expansive inlining"
		},
	      Controls.control {
		  ctl = inlineDebug,
		  name = "inline-debug",
		  pri = [0, 1],
		  obscurity = 0,
		  help = "debug expansive inlining"
		}
	    ]

  (********** Counters for statistics **********)
    val cntBeta                 = ST.newCounter "cps-inline:beta"
    val cntBetaCont             = ST.newCounter "cps-inline:beta-cont"
    val cntFOInline             = ST.newCounter "cps-inline:first-order-inline"
    val cntHOinline             = ST.newCounter "cps-inline:higher-order-inline"
    val cntSafe                 = ST.newCounter "cps-inline:safe"
    val cntUnsafe               = ST.newCounter "cps-inline:unsafe"

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
    fun inc x = CV.addToCount(x, 1)
    fun dec x = CV.addToCount(x, ~1)
    val dec' = List.app dec
    fun unused x = (useCntOf x = 0)

  (* Variable renaming *)
    fun rename (env, x, y) = (
	(* every use of x will be replaced by a use of y *)
	  combineAppUseCnts(y, x);
	  VMap.insert(env, x, y))

    fun rename' (env, [], []) = env
      | rename' (env, x::xs, y::ys) = rename' (rename(env, x, y), xs, ys)
      | rename' _ = raise Fail "rename': arity mismatch"

  (* apply a substitution to a variable *)
    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))

  (* apply a substitution to a list of variables *)
    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst(env, x) :: subst'(env, xs)

  (* decrement a variable's use count after applying a substitution *)
    fun substDec (env, x) = (case VMap.find(env, x)
	   of SOME y => dec y
	    | NONE => dec x
	  (* end case *))

  (* extend the environment with a mapping from the "toVars" to the "fromVars" (i.e.,
   * instances of a ftoVarromVar will be replaced with the corresponding fromVar),
   * inserting type casts as necessary
   *)
    fun extendWithCasts {env, fromVars, toVars} = let
        (* FIXME -- Do this right! *)
          fun needsCast (fromTy, toTy) = (case fromTy
                 of CTy.T_Any => not (CPSTyUtil.equal (CTy.T_Any, toTy)) 
                  | CTy.T_Tuple(b, ts) => (case toTy
                       of CTy.T_Tuple (b', ts') => ListPair.exists needsCast (ts, ts')
                        | _ => false
                      (* end case *))
                  | _ => false
                (* end case *))
          fun mkCasts ([], [], fromVars', casts) = (List.rev fromVars', List.rev casts)
            | mkCasts (_::_, [], _, _) = raise Fail "more fromVars than toVars"
            | mkCasts ([], _::_, _, _) = raise Fail "more toVars than fromVars"
            | mkCasts (fromVar::fromVars, toVar::toVars, fromVars', casts) = let
                val fromTy = CV.typeOf fromVar
                val toTy = CV.typeOf toVar
                in
                  if not (needsCast (fromTy, toTy))
                    then mkCasts (fromVars, toVars, fromVar::fromVars', casts)
                    else let
                      val name = let val x = CV.nameOf fromVar
                            in 
                              concat ["_cast", (if String.isPrefix "_" x then "" else "_"), x]
                            end
                      val c = CV.new (name, toTy)
                      val _ = Census.incUseCnt c (* because bind will decrement the count *)
                      val cast = ([c], C.Cast(toTy, fromVar))
                      in
                        mkCasts (fromVars, toVars, c::fromVars', cast::casts)
                      end
                end
          val (fromVars',casts) = mkCasts (fromVars, toVars, [], [])
          fun bind (fromVar', toVar, env) = (
                CV.combineAppUseCnts (fromVar', toVar);
                dec fromVar';
                VMap.insert (env, toVar, fromVar'))
          val env' = ListPair.foldl bind env (fromVars', toVars)
          in
	    (env', casts)
          end

  (********** The inlining environment **********)

    val scale = 2
    val initK = 10

  (* two-step decimation function, but we never go below a scale of 1 *)
    (* TODO: won't this only ever be 10 or 5? *)
    fun inldec k = if (k = initK) then k div scale else 1

  (* the environment used for making inlining decisions.  The factor k is
   * used scale the size of the application, while the set s contains those
   * functions that should not be inlined; either because we are inlining
   * them or we are in their body.
   * We also need to track all of the bound variables to ensure they are
   * available for functions about to be inlined.
   *)
    datatype env = E of {k : int, s : VSet.set, env : VSet.set}

  (* add the function f to the environment *)
    fun addFun (E{k, s, env}, f) = E{k=k, s=VSet.add(s, f), env=env}

  (* add the function f and decrement the size factor *)
    fun addAndDec (E{k, s, env}, f) = E{k=inldec k, s=VSet.add(s, f), env=env}

    (* Extend the environment with another bound variable *)
    fun extend (E{k, s, env}, v) = E{k=k, s=s, env=VSet.add(env, v)}
    fun extend' (e, vs) = List.foldl (fn (v, e) => extend (e, v)) e vs

  (* the initial envronment *)
    fun initEnv () = E{k = initK, s = VSet.empty, env = VSet.empty}


  (********** Inlining **********)

    (*
     * Inlining is safe when either:
     * a) there are no free variables
     * b) there are free variables, but we have analyzed that point
     * in the program and know that the proper reflow property holds.
     * We may not know this property in the case where we are analyzing
     * the body of a just-inlined function. This restriction means
     * that this is not a "complete" inlining.
     * 
     *)
    fun isSafe (pptInlineLocation, lambda, env) = let
        val CPS.FB{f,...} = lambda
        val fvs = FreeVars.envOfFun f
        fun unsafeFV fv = let
            val funLoc = Reflow.bindingLocation f
            val fvLoc = Reflow.bindingLocation fv
            val result = (Reflow.pathExists (funLoc, fvLoc)) andalso
                         (Reflow.pathExists (fvLoc, pptInlineLocation))
        in
            if !inlineDebug
            then ((if result
                   then print (concat [CV.toString fv, " is unsafe in ", CV.toString f, ".\n"])
                   else print (concat [CV.toString fv, " is safe in ", CV.toString f, ".\n"]));
                  result)
            else result
        end
        val _ = if !inlineHOFlg andalso not(VSet.isSubset(fvs, env))
                then (print (concat [CV.toString f, " could not be inlined due to missing FVs.\n"]);
                      VSet.app (fn x => print (concat[" -- ", CV.toString x, "\n"]))
                               (VSet.difference (fvs, env)))
		else ()
    in
        if !inlineHOFlg
        then VSet.numItems fvs = 0
        else (
            VSet.numItems fvs = 0 orelse
            (Reflow.pointAnalyzed pptInlineLocation andalso
             VSet.isSubset(fvs, env) andalso
             not(VSet.exists unsafeFV fvs)))
    end

  (* test to see if a function application ``f(args / rets)'' should be
   * inlined.  If so, return SOME(fb), where fb is the lambda
   * bound to f, otherwise return NONE.
   *)
    fun shouldInlineApp (E{k, s, env}, ppt, f, args, rets) = (
        case CV.kindOf f
	 of C.VK_Fun(fb as C.FB{body, ...}) =>
	    if not(VSet.member (s, f)) andalso
               Sizes.smallerThan(body, k * Sizes.sizeOfApply(f, args, rets))
	    then (ST.tick cntFOInline;
                  SOME fb)
	    else NONE
	  | _ => (case CFA.valueOf f
                   of CFA.LAMBDAS (l) => (
                       case CV.Set.listItems l
                        of [f'] => let
                            val C.VK_Fun (fb as C.FB{body,...}) = CV.kindOf f'
                        in
                            if not(VSet.member (s, f')) andalso
                               not(CFA.isProxy f') andalso
                               isSafe (ppt, fb, env) andalso
                               Sizes.smallerThan(body, k * Sizes.sizeOfApply(f, args, rets))
                            then (ST.tick cntHOinline;
                                  SOME fb)
                            else NONE
                        end
                         | _ => NONE)
                    | _ => NONE))

    fun shouldInlineThrow (E{k, s, env}, ppt, f, args) = (
        case CV.kindOf f
	 of C.VK_Cont(fb as C.FB{body, ...}) => 
	    if not(VSet.member (s, f)) andalso
               Sizes.smallerThan(body, k * Sizes.sizeOfThrow(f, args))
	    then (ST.tick cntFOInline;
                  SOME fb)
	    else NONE
	  | _ => (case CFA.valueOf f
                   of CFA.LAMBDAS (l) => (
                       case CV.Set.listItems l
                        of [f'] => let
                            val C.VK_Cont (fb as C.FB{body,...}) = CV.kindOf f'
                        in
                            if not(VSet.member (s, f')) andalso
                               not(CFA.isProxy f') andalso
                               isSafe (ppt, fb, env) andalso
                               Sizes.smallerThan(body, k * Sizes.sizeOfThrow(f, args))
                            then (ST.tick cntHOinline;
                                  SOME fb)
                            else NONE
                        end
                         | _ => NONE)
                    | _ => NONE))

    fun doExp (env, exp as C.Exp(ppt, e)) = (case e
	   of C.Let(lhs, rhs, e) =>
              C.Exp (ppt, C.Let (lhs, rhs, doExp (extend'(env, lhs), e)))
	    | C.Fun(fbs, e) => let
                val funs = List.map (fn (C.FB{f,...}) => f) fbs
                val env = extend'(env, funs)
		val e = doExp(env, e)
		fun doFB (C.FB{f, params, rets, body}) = let
                    val env = extend' (env, params)
                    val env = extend' (env, rets)
                in
                    C.FB{f=f, params=params, rets=rets,
			 body=doExp(env, body)}
                end
	      (* note that the mkLambda resets the kind info *)
		val fbs = List.map (fn x => C.mkLambda (x,false)) (List.map doFB fbs)
		in
                    C.Exp (ppt, C.Fun (fbs, e))
		end
	    | C.Cont(fb as C.FB{f, params, rets, body}, e) => let
                  val env = extend(env, f)
		  val e = doExp (env, e)
		  fun doFB (C.FB{f, params, rets, body}) = let
                      val env = extend' (env, params)
                      val env = extend' (env, rets)
                  in
                      C.FB{f=f, params=params, rets=rets,
			   body=doExp(env, body)}
                  end
		  val fb = doFB fb
		  val _ = setBinding(f, C.VK_Cont fb)
		  in
                      C.Exp (ppt, C.Cont (fb, e))
		  end
	    | C.If(cond, e1, e2) => C.Exp (ppt, C.If(cond,
		doExp(env, e1),
		doExp(env, e2)))
	    | C.Switch(x, cases, dflt) =>
              C.Exp (ppt, C.Switch (x,
		          List.map (fn (l, e) => (l, doExp(env, e))) cases,
		          Option.map (fn e => doExp(env, e)) dflt))
	    | C.Apply(f, args, conts) => (
                case shouldInlineApp (env, ppt, f, args, conts)
                 of SOME (C.FB{f, params, rets, body}) => (
                     ST.tick cntBeta;
                     doInline (env, f, conts@args, rets@params, body))
                  | NONE => C.Exp (ppt, C.Apply (f, args, conts)))
	    | C.Throw(k, args) => (
                case shouldInlineThrow (env, ppt, k, args)
                 of SOME (C.FB{f, params, body, ...}) => (
                     ST.tick cntBetaCont;
                     doInline (env, f, args, params, body))
                  | NONE => C.Exp (ppt, C.Throw (k, args)))
	  (* end case *))

    and doInline (env, f, args, params, body) = let
	(* Extend the substitution to map the parameter to the argument and decrement
	 * the argument use count.
         *
         * CONSIDER: we inline the old definition of the function, which could possibly
         * have some of its own applications that have now been subject to inlining.
         * Should we instead replace with the new definition? Or perform inlining
         * iteratively, to cleanly handle the recursive function definitions case?
	 *)
        val (argsForParams, casts) = extendWithCasts {env = U.empty, fromVars = args, toVars = params}
    in
	C.mkLets (casts, doExp (addAndDec (env, f), U.copyExp (argsForParams, body)))
    end

    fun transform (m as C.MODULE{name, externs, body}) =
        if !inlineFlg
        then (let
                 val _ = if !inlineHOFlg
                         then Reflow.analyze m
                         else ()
	         val C.FB{f, params, rets, body} = body
	         val body = doExp(initEnv(), body)
	         val fb = C.mkLambda(C.FB{f=f, params=params, rets=rets, body=body}, false)
                 val m = C.MODULE{name=name, externs=externs, body=fb}
                 val _ = Census.census m
             in
                 m
             end)
        else m
  end
