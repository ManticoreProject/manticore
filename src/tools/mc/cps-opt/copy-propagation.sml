(* copy-propagation.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This transformation replaces any variables that are not a function definition
 * but which CFA tells us can only be bound to a single function with that function's
 * literal name. Note that we need to respect lexical scoping (which the CFA analysis
 * does not require to be true).
 * We also need to be environmentally consonant (Shivers' term) - we can only replace
 * a function if we're replacing it with one that is from an environment guaranteed
 * to have the same bindings. This prevents replacing a call to a closure with a
 * variable bound to a value A from being replaced with a call to the known function
 * in an environment where the variable is bound to a value B.
 *
 * TODOs:
 * Split analysis from transformation. !pass is a horrible hack
 *)

structure CopyPropagation : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure PPt = ProgPt
    structure C = CPS
    structure CV = C.Var
    structure VSet = CV.Set
    structure VMap =  CV.Map
    structure U = CPSUtil
    structure CTy = CPSTy
    structure CFA = CFACPS
    structure ST = Stats

  (***** controls ******)
    val enableCopyPropagation = ref false
    val propagationDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = enableCopyPropagation,
                  name = "copy-propagation",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable copy propagation"
                },
              Controls.control {
                  ctl = propagationDebug,
                  name = "copy-propagation-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug copy propagation"
                  }
            ]

    datatype copyResult = MOVE of CV.var | COPY of CV.var | SKIP

    (***** Statistics *****)
    val cntPropagatedFunctions  = ST.newCounter "cps-copy-propagation:propagated-functions"
    val cntHoistedFunctions     = ST.newCounter "cps-copy-propagation:hoisted-functions"

    (* Type used for building a function nesting hierarchy
     * This is used to determine a common-parent (LUB) when hoisting
     * functions up.
     *)
    local
      val {setFn, getFn, ...} = CV.newProp (fn f => (f, 0))
    in
    fun setParent (f : CV.var, (p : CV.var, l : int)) = (
        if !propagationDebug
        then print (concat ["Parent function of ", CV.toString f,
                            " is ", CV.toString p, " at level ",
                            Int.toString l, "\n"])
        else ();
        setFn (f, (p, l)))
    fun getParent f = getFn f
    end

    fun fixup (translations) = let
        (* Find the common parent of the two *)
        fun fixOne (callee, caller, first) =
            if CV.compare (callee, caller) = EQUAL
            then caller
            else let
                    val (calleeParent , calleeLevel) = getParent callee
                    val (callerParent, callerLevel) = getParent caller
                in
                    case Int.compare (calleeLevel, callerLevel)
                     of EQUAL => ( (* If the original caller/callee
                                    * have the same parent, then try to just
                                    * shuffle ahead instead of up. *)
                        if CV.compare (calleeParent, callerParent) = EQUAL
                           andalso first
                        then caller
                        else fixOne (calleeParent, callerParent, false))
                      | GREATER => fixOne (calleeParent, caller, false)
                      | LESS => fixOne (callee, callerParent, false)
                end
    in
        VMap.mapi (fn (a,b) => fixOne (a,b,true))  translations
    end


    (* property for tracking bindings to function bodies
     *)
    local
      val {setFn, getFn, ...} = CV.newProp (fn f => raise Fail "Undefined function binding during copy propagation.")
    in
    fun setFB (f,b : C.lambda) = setFn (f, b)
    fun getFB f = getFn f
    end

    fun isClosed (lambda as C.FB{f,params,rets,body}, env) = let
        val env = VSet.addList (env, params)
        val env = VSet.addList (env, rets)
        val env = VSet.add (env, f)
        fun checkMembership (env, var) = let
            val isMember = VSet.member (env, var)
        in
            if (!propagationDebug) andalso not(isMember)
            then (print (concat [CV.toString var, " is free in attempted hoist.\n"]) ; isMember)
            else isMember
        end
        fun checkList (env, l) = List.foldl (fn (a,b) => b andalso checkMembership (env, a)) true l
        fun isClosedExp (C.Exp(ppt, e), env) = isClosedTerm (e, env)
        and isClosedTerm (C.Let (lhs, rhs, body), env) = let
            val env = VSet.addList (env, lhs)
        in
            isClosedRHS (rhs, env) andalso isClosedExp (body, env)
        end
          | isClosedTerm (C.Fun (lambdas, body), env) = let
                val (b, env) = List.foldr (fn (f,(b,e)) => let val (b',e') = isClosedLambda (f,e) in
                                                               (b andalso b', e') end) (true,env) lambdas
            in
                b andalso isClosedExp (body, env)
            end
          | isClosedTerm (C.Cont (lambda, body), env) = let
                val (b,env) = isClosedLambda (lambda, env)
            in
                b andalso isClosedExp (body, env)
            end
          | isClosedTerm (C.If (cond, e1, e2), env) = checkList (env, CondUtil.varsOf cond) andalso
            isClosedExp (e1, env) andalso isClosedExp (e2, env)
          | isClosedTerm (C.Switch (x, cases, default), env) =
            checkMembership (env, x) andalso
            (List.foldl (fn ((tag,body),b) => b andalso isClosedExp (body, env)) true cases) andalso
            (case default of SOME(e) => isClosedExp(e, env) | NONE => true)
          | isClosedTerm (C.Apply (f, args, params), env) =
            checkMembership (env, f) andalso
            checkList (env, args) andalso
            checkList (env, params)
          | isClosedTerm (C.Throw (k, args), env) =
            checkMembership (env, k) andalso
            checkList (env, args)
        and isClosedRHS (C.Var (vars), env) = checkList (env, vars)
          | isClosedRHS (C.Cast (_, v), env) = checkMembership (env, v)
          | isClosedRHS (C.Const (_, _), _) = true
          | isClosedRHS (C.Select (_, v), env) = checkMembership (env, v)
          | isClosedRHS (C.Update (_, v1, v2), env) = checkMembership (env, v1) andalso checkMembership (env, v2)
          | isClosedRHS (C.AddrOf (_, v), env) = checkMembership (env, v)
          | isClosedRHS (C.Alloc (_, vars), env) = checkList (env, vars)
          | isClosedRHS (C.Promote (v), env) = checkMembership (env, v)
          | isClosedRHS (C.Prim (prim), env) = checkList (env, PrimUtil.varsOf prim)
          | isClosedRHS (C.CCall (v, vars), env) = checkMembership (env, v) andalso checkList (env, vars)
          | isClosedRHS (C.HostVProc, _) = true
          | isClosedRHS (C.VPLoad (_, v), env) = checkMembership (env, v)
          | isClosedRHS (C.VPStore (_, v1, v2), env) = checkMembership (env, v1) andalso checkMembership (env, v2)
          | isClosedRHS (C.VPAddr (_, v), env) = checkMembership (env, v)
        and isClosedLambda (lambda as C.FB{f,params,rets,body}, env) = let
            val env = VSet.add (env, f)
            val env' = VSet.addList (env, params)
            val env' = VSet.addList (env', rets)
        in
            (isClosedExp (body, env'), env)
        end
    in
        isClosedExp (body, env)
    end

    fun copyPropagate (C.MODULE{name,externs,body=(mainLambda
                                                       as C.FB{f=main,params=modParams,rets=modRets,body=modBody})}) = let
        val pass = ref 0
        (* Only insert into the map if the callee isn't already being moved up.
         * If it has already been scheduled for a move, that will also 'work' for the later
         * one.
         * We handle cycles later, by refusing to move a function that has already been moved.
         *)
	val externsEnv = List.foldl
		             (fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		             VSet.empty externs
        fun insert (map, callee, caller) = (
            case VMap.find (map, callee)
             of NONE => VMap.insert (map, callee, caller)
              | _ => map
        (* end case *))
        fun findCopy (f, env, parent) = (
            case CFA.valueOf f
             of CFA.LAMBDAS (l) => (
                case CV.Set.listItems l
                 of [f'] =>
                    (if CV.compare (f, f') = EQUAL
                     then SKIP
                     else
                         (if VSet.member (env, f')
                          then if !pass = 1
                               then let
                                       (* TODO: this is far too conservative - instead compute whether the
                                        * environment contains bindings set on the same call contour (reflow)
                                        *)
                                       val isSafe = isClosed (getFB f', externsEnv)
                                   in
                                       if isSafe
                                       then (if !propagationDebug
                                             then print (concat [CV.toString f', " is being propagated.\n"])
                                             else ();
                                             COPY (f'))
                                       else (print (concat [CV.toString f',
                                                            " was not safe for subst for copy-prop in place of ",
                                                            CV.toString f, ".\n"]);
                                             SKIP)
                                   end
                               else SKIP (* Only make changes in the second pass *)
                             else (if !propagationDebug
                                   then (print (concat [CV.toString f', " was not in scope for copy-prop over ",
                                                        CV.toString parent, " in pass ", Int.toString (!pass), ".\n"]))
                                   else ();
                                   MOVE(f'))))
                  | _ => SKIP
                (* end case *))
              | _ => SKIP
        (* end case *))
        fun wrapWithNewPreds (lambdas, env, map, continue) =
            if (!pass) = 0
            then continue ((fn x => x), env, map)
            else let
                    fun wrapWithNewPreds' (l'::rest, wrapper, env, map, continue) = let
                        val C.FB{f=f',...} = l'
                        val preds = List.map (fn (k,v) => getFB k)
                                             (VMap.listItemsi
                                                  (VMap.filter
                                                       (fn a => (CV.compare (a,f')=EQUAL)) map))
                        fun wrapFun ((p as C.FB{f,...})::preds, wrapper, env, map) =
                            if isClosed (p, env) andalso not(VSet.member (env, f))
                            then let
                                    val (l as C.FB{rets,...}, env, map) = copyPropagateLambda (p, env, map)
                                    val _ = if !propagationDebug
                                            then print (concat [CV.toString f, " is being hoisted above ",
                                                                CV.toString f', ".\n"])
                                            else ()
                                in
                                    ST.tick cntHoistedFunctions;
                                    case rets
                                     of r::rs => wrapFun (preds, (fn x => C.mkFun ([l], x)) o wrapper,
                                                         env, map)
                                      | [] => wrapFun (preds, (fn x => C.mkCont (l, x)) o wrapper,
                                                       env, map)
                                end
                            else (if !propagationDebug
                                  then print (concat [CV.toString f, " could not be hoisted above ",
                                                      CV.toString f', ".\n"])
                                  else ();
                                  wrapFun (preds, wrapper, env, map))
                          | wrapFun ([], wrapper, env, map) = wrapWithNewPreds' (rest, wrapper, env, map, continue)
                    in
                        (* For each potential predecessor, IF it's closed at the new location
                         * THEN emit it here; run copyPropagate on it; push new env+map along.
                         *)
                        wrapFun (preds, wrapper, env, map)
                    end
                      | wrapWithNewPreds' ([], wrapper, env, map, continue) = continue (wrapper, env, map)
                in
                    wrapWithNewPreds' (lambdas, (fn x => x), env, map, continue)
                end
            
            
        and copyPropagateExp (exp as C.Exp(ppt, e), env, map, parent) = (
            case e
             of C.Let (vars, rhs, e) => let
                    val env' = VSet.addList(env, vars)
                    val (body, env'', map') = copyPropagateExp (e, env', map, parent)
                in (C.mkLet (vars, rhs, body), env'', map') end
              | C.Fun (lambdas, body) => (
                if !pass=0
                then let
                        val (_, n) = getParent parent
                    in
                        List.app (fn (l as C.FB{f,...}) => setParent (f, (parent, n+1))) lambdas
                    end
                else ();
                wrapWithNewPreds (
                lambdas, env, map,
                (fn (wrapper, env, map) => let
                        fun propLambda(l as C.FB{f,...}, (ls, env, map)) =
                            if VSet.member (env, f)
                            then (ls, env, map)
                            else let
                                    val (l', env', map') = copyPropagateLambda (l, env, map)
                                in
                                    (l'::ls, env', map')
                                end
                        val (lambdas, env', map') = List.foldr propLambda
                                                               ([], env, map) lambdas
                        val (body, _, map'') = copyPropagateExp (body, env', map', parent)
                    in
                        if List.null lambdas
                        then (wrapper body, env', map'')
                        else (wrapper (C.mkFun(lambdas, body)), env', map'')
                    end)))
              | C.Cont (f as C.FB{f=fname,...}, body) =>(
                if !pass=0
                then let
                        val (_, n) = getParent parent
                    in
                        setParent (fname, (parent, n+1))
                    end
                else ();
                wrapWithNewPreds (
                [f], env, map,
                (fn (wrapper, env, map) =>
                    if VSet.member (env, fname)
                    then let
                            val (body, _, map'') = copyPropagateExp (body, env, map, parent)
                        in
                            (wrapper body, env, map'')
                        end
                    else let
                            val (lambda, env', map') = copyPropagateLambda (f, env, map)
                            val (body, _, map'') = copyPropagateExp (body, env', map', parent)
                        in
                            (wrapper (C.mkCont (lambda, body)), env', map'')
                        end)))
              | C.If (v, e1, e2) => let
                    val (e1', _, map') = copyPropagateExp (e1, env, map, parent)
                    val (e2', _, map'') = copyPropagateExp (e2, env, map', parent)
                in
                    (C.mkIf (v, e1', e2'), env, map'')
                end
              | C.Switch (v, cases, body) => let
                    val (switches, map') = List.foldr (fn ((tag, e), (rr, map)) => let
                                                              val (body, _, map') = copyPropagateExp (e, env, map, parent)
                                                          in ((tag, body)::rr, map') end) ([], map) cases
                    val (default, _, map') = (case body
                                               of SOME(x) => let
                                                      val (e, v, map') = copyPropagateExp (x, env, map, parent)
                                                  in (SOME(e), v, map') end
                                                | NONE => (NONE, VSet.empty, map))
                in
                    (C.mkSwitch(v, switches, default), env, map')
                end
              | C.Apply (f, args, retArgs) => (
                case findCopy (f, env, parent)
                 of COPY (f') => (ST.tick cntPropagatedFunctions;
                                  Census.decAppCnt f;
                                  Census.incAppCnt f';
                                  (C.mkApply (f', args, retArgs), env, map))
                  | MOVE(callee) => (C.mkApply (f, args, retArgs), env,
                                     insert (map, callee, parent))
                  | SKIP => (C.mkApply (f, args, retArgs), env, map)
                (* end case *))
              | C.Throw (k, args) => (
                case findCopy (k, env, parent)
                 of COPY (k') => (ST.tick cntPropagatedFunctions;
                                  Census.decAppCnt k;
                                  Census.incAppCnt k';
                                  (C.mkThrow (k', args), env, map))
                  | MOVE(callee) => (C.mkThrow (k, args), env, insert (map, callee, parent))
                  | SKIP => (C.mkThrow (k, args), env, map)
                (* end case *))
        (* end case *))
        and copyPropagateLambda (lambda as C.FB{f, params, rets, body}, env, map) = let
            val _ = if (!pass) = 0 then setFB (f, lambda) else ()
            val _ = if VSet.member (env, f) then (raise Fail (concat[CV.toString f, " is already a member"])) else ()
            val env' = VSet.add (env, f)
            val env' = VSet.addList (env', params)
            val env' = VSet.addList (env', rets)
            val (body, _, map') = copyPropagateExp (body, env', map, f)
        in
            (C.mkLambda(C.FB{f=f,params=params,rets=rets,body=body}), env', map')
        end
        val env = VSet.add (externsEnv, main)
        (* In the first pass, gather desired moves *)
        val (_, _, translations) = copyPropagateExp (modBody, env, VMap.empty, main)
        (* On the second time through, do copy prop and make any moves we can *)
        val _ = pass := !pass +1
        val translations = fixup (translations)
        val (body', _, _) = copyPropagateExp (modBody, env, translations, main)
    in
        C.MODULE{
	name=name, externs=externs,
	body = C.mkLambda(C.FB{
                          f=main,params=modParams,rets=modRets,
                          body=body'
		         })
	}
    end

    fun transform m =
        if !enableCopyPropagation
	then copyPropagate m
        else m

  end
