(* wrap-captures.sml
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * After cont classification for direct-style conversion, we wrap continuation
 * captures so they may be emitted correctly by the code generator. In particular,
 * we perform the following type of eta-expansion
 *
 *  fun outerF(.. / retK : cont(t', t'', ...)) =
 *    A
 *    cont k (x : t) = B   <- an Other or Goto cont.
 *    in
 *      C
 *
 *      ===>
 *
 *  fun outerF (.. / retK : cont(t', t'', ...)) =
 *    A
 *    cont k (x : t) = B                                    <-- now classified as a join cont!
 *    in
 *      cont landingPad (regularRet : bool, arg : any) =    <-- a ret cont
 *       if regularRet
 *         then throw retK (#1(arg), #2(arg), ...)
 *         else throw k arg
 *      in
 *        fun manipK (k' : cont(t) / landingPad' : cont(bool, any), deadExh) =
 *          cont manipRetk (a1, a2, ...) =                  <-- a join/ret cont
                let x = alloc(a1, a2, ...)                  <-- only if more than 1 arg
 *              throw landingPad' (true, x)
 *          in
 *            [ C / k -> k', retK -> manipRetk ]
 *        (* end manipK *)
 *        callec ( manipK / landingPad, deadExh)
 *
 *  the callec construct generates code that calls a special shim function.
 *  The shim will allocate the following continuation to represent 'k':
 *
 *  - [ ASM_Resume | stackPtr (@landingPad) | stackInfo | false ]
 *    When invoked, the closure passes the arg its given
 *    to the stack frame pointed to, passing "false" in addition
 *    to jump to the right block.
 *
 *  - during translation, within the expression C, any 'throw retK (val)'
 *    is changed to 'throw retK' [true, val]'
 *
 *
 *)


 (* TODO FIXME

    Consider the following

    cont joinK () = ...
    in
      cont otherK () = ...
      in
        expA   <- contains a throw to joinK

    ===>

    cont joinK () = ...    <-- uh oh, now this is a GotoCont!
    in
      cont otherK () = ...
      in
        cont landingPad () ...
        in
          fun manipK( , landingPad' ) =
            ...
            throw joinK ()   <-- now a Goto throw.

    the solution to this is to ask for the FVs of expA and check for any Join conts
    in that set. For each one, assign to it an integer.
    Then, in the landingPad, have a switch dispatch to these continuations based
    on that integer, and replace all throws to those joinK's with a throw to the landing pad,
    as appropriate. You could introduce sentinel conts in manipK that pick the right integer
    for each one to hide calling convention issues.

    Because this is a rare thing to happen in my benchmarking, i.e., it only showed up
    when I was trying to compile the round-robin scheduler, which has needless uses of
    conts to escape loops, I haven't bothered to update the code below for this case.
    - Kavon (3/19/17)

 *)

structure WrapCaptures : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure VSet = CV.Set
    structure ST = Stats
    structure L = List
    structure K = ClassifyConts

    (********** Counters for statistics **********)
    val cntExpand = ST.newCounter "wrap-captures:expand"

    (***** environment utils *****)
    datatype environment = E of { sub : cont_kind VMap.map, retk : CV.var, paramRetk : CV.var, manipScope : bool }
        and cont_kind = RetCont of CV.var
                      | EscapeCont of CV.var

    fun emptyEnv () = E{ sub = VMap.empty,
                         retk = CV.new("wrongRetk", CPSTy.T_Any),
                         paramRetk = CV.new("wrongRetk", CPSTy.T_Any),
                         manipScope = false
                       }

    (* get and set the "active" retk *)
    fun setRet ((E{sub, retk, paramRetk, manipScope}), r) =
        E{sub=sub, retk=r, paramRetk=paramRetk, manipScope=manipScope}
    fun getRet (E{retk,...}) = retk

    (* get and set the enclosing function's retk; for the hack in Apply. *)
    fun setParamRet ((E{sub, retk, paramRetk,manipScope}), r) = E{sub=sub, retk=retk, paramRetk=r, manipScope=manipScope}
    fun getParamRet (E{paramRetk,...}) = paramRetk

    (* finds the latest substitution in the env *)
    fun lookupKind (E{sub,...}, v) = let
        fun some tycon v = SOME(tycon v)
        fun get (tycon, v) = (case VMap.find(sub, v)
            of SOME(RetCont newV) => get (some RetCont, newV)
             | SOME(EscapeCont newV) => get (some EscapeCont, newV)
             | NONE => tycon v
            (* esac *))
    in
        get (fn _ => NONE, v)
    end


    (* finds the latest substitution in the env *)
    and lookupV(env as E{sub,...}, v) = (case VMap.find(sub, v)
        of SOME(RetCont newV) => lookupV(env, newV)
         | SOME(EscapeCont newV) => lookupV(env, newV)
         | NONE => v)

    fun inManipScope (E{manipScope,...}) = manipScope
    fun setManipScope (E{sub, retk, paramRetk, manipScope}, flag) =
        E{sub=sub, retk=retk, paramRetk=paramRetk, manipScope=flag}

    fun insertV (E{sub,retk,paramRetk,manipScope}, var, valu) =
        E{sub = VMap.insert(sub, var, valu), retk=retk, paramRetk=paramRetk, manipScope=manipScope}

    fun subst env v = lookupV(env, v)

    (***** end of environment utils *****)


    (* I would use bool but I don't want to mess with the enum stuff. *)
    val indicatorTy = CPSTy.T_Raw(RawTypes.T_Int)
    val falseVal = Literal.Int 0
    val trueVal = Literal.Int 1


    (****** helper funs to build expressions ******)
    structure MK = struct
        fun fresh(ty, k) = k (CV.new("t", ty))

        fun cast(var, targTy, k) = let
            val lhs = CV.new(CV.nameOf var, targTy)
        in
            C.mkLet([lhs], C.Cast(targTy, var), k lhs)
        end

        fun atLeastOneArg (args, k) =
            if L.null args
            then fresh(CPSTy.T_Any, fn argv =>
                    C.mkLet([argv], C.Const(Literal.unitLit, CPSTy.T_Any),
                        k [argv]))
            else k args


        fun selectAll(tys, tup, k) = let
            fun get (_, [], args) = k (L.rev args)
              | get (i, t::ts, args) =
                    fresh(t, fn a =>
                      C.mkLet([a], C.Select(i, tup), get(i+1, ts, a::args)))
        in
            get(0, tys, [])
        end

        (* this should match up with 'dispatch' below *)
        fun bundle ([], k) = k [] (* NOTE kind of unexpected but okay *)
          | bundle ([a], k) = k [a]
          | bundle (args, k) = let (* we need to bundle them up *)
                val allocTy = CPSTy.T_Tuple(false, L.map CV.typeOf args)
                val lhs = CV.new("bundle", allocTy)
              in
                C.mkLet([lhs], C.Alloc(allocTy, args), k [lhs])
              end

        fun dummyExh k = let
            val exhTy = CPSTy.T_Cont([CPSTy.T_Any])
            val exh = CV.new("deadExh", CPSTy.T_Any)
        in
            C.mkLet([exh], C.Const(Literal.unitLit, CPSTy.T_Any),
              cast(exh, exhTy, k))
        end

        fun unitRetk(ty, k) = let
            val retk = CV.new("unitRetk", CPSTy.T_Any)
        in
            C.mkLet([retk], C.Const(Literal.unitLit, CPSTy.T_Any),
              cast(retk, ty, k))
        end

        fun bindTrue k =
            fresh(indicatorTy, fn lhs =>
                C.mkLet([lhs], C.Const(trueVal, indicatorTy),
                    k lhs))

        fun argTysOf cont = (case CV.typeOf cont
            of CPSTy.T_Cont tys => tys
             | _ => raise Fail "not a cont")

    end (* end MK *)


    fun doExp (env, C.Exp(ppt, t)) = let
        fun wrap term = C.Exp(ppt, term)
    in
    (case t
        of C.Let (vars, rhs, e) => wrap (C.Let(vars, CPSUtil.mapRHS (subst env) rhs, doExp(env, e)))
         | C.Fun (lambdas, e) => wrap (C.Fun(L.map (doFun env) lambdas, doExp(env, e)))
         | C.If (cond, e1, e2) => wrap (C.If (cond, doExp(env, e1), doExp(env, e2)))
         | C.Switch(var, arms, dflt) => wrap (let
                fun doArm (tag, e) = (tag, doExp(env, e))
                fun doDflt e = doExp(env, e)
             in
                C.Switch(var, L.map doArm arms, Option.map doDflt dflt)
             end)
         | C.Apply (f, args, rets) => let
                (* an Apply may change from being in tail position
                   to non-tail due to manipKRetk, which are the only types of retk's
                   in the environment in the case of a substution in an Apply. *)

                val newRetk = ref false

                (* it turns out that if the retk was eliminated, then it is
                   replaced with unit casted to the type of the retk in the enclosing
                   fun. This becomes a problem for us because when we wrap C
                   with manipK, the type of the retk changes. So a simple substitute
                   becomes ineffective. Thus, we specifically detect this case and
                   change the retk to the currently active retk (as opposed to the enclosing fun's). *)
                fun substRets (env, [retk, exnk], k) =
                        replaceRetk(env, retk, fn newRetk => k [newRetk, exnk])
                  | substRets (env, [retk], k) =
                        replaceRetk(env, retk, fn newRetk => k [newRetk])

                and replaceRetk (env, oldRetk, k) = (case lookupKind(env, oldRetk)
                    of SOME(EscapeCont _) => raise Fail (CV.toString oldRetk ^ " should not appear as a ret!")
                     | SOME(RetCont newV) => k (newRetk := true ; newV)
                     | NONE => let
                        (* check if the oldRetk is unit *)
                        fun isConst v = (case CV.kindOf v
                            of C.VK_Let(C.Cast(_, v)) => isConst v
                             | C.VK_Let(C.Const _) => true
                             | _ => false
                            (* esac *))

                        val curRet = getRet env
                        val paramRet = getParamRet env
                        (* DEBUG
                        val _ = print ("inspecting: "
                                    ^ CV.toString oldRetk
                                    ^ " VS paramRet " ^ CV.toString paramRet
                                    ^ " VS curRet " ^ CV.toString curRet ^ " ... \n")
                        *)
                     in
                        if inManipScope env
                            andalso isConst oldRetk
                            andalso CPSTyUtil.match(CV.typeOf oldRetk, CV.typeOf curRet)

                        then k (newRetk := true ; curRet)

                        else k oldRetk
                     end
                     (* esac *))
             in
                substRets(env, rets, fn rets => let
                        val applyExp = wrap (C.Apply(f, L.map (subst env) args, rets))
                    in
                        if !newRetk
                        then (K.setTailApply(applyExp, false) ; applyExp)
                        else applyExp
                    end)
             end

         | C.Throw (k, args) => wrap(C.Throw(subst env k, L.map (subst env) args))

         | C.Cont (C.FB{f, params, rets, body}, e) => wrap (case K.kindOfCont f
             of (K.GotoCont | K.OtherCont) => let   (* TODO change the classification of f, set the classification of retkWrap *)

                    (* The reason for the check below is that this continuation has unknown call sites.
                       By the nature of how these continuations are captured in direct-style, we
                       must merge the types of the parameters to the return continuation and
                       this continuation so they have the same calling convention (both must return
                       to the same basic block in the end, just like setjmp/longjmp).

                       We can (and do) eta-expand the throw to the original return cont to match up
                       the calling convention with the landing pad. However, we _cannot_ do the
                       same for this escape cont, because then the cont we wrapped around the throw
                       would be used as a Goto/Other cont, and we're back where we started!

                       If we were to simply change every Throw exp using an escape cont, by bundling up
                       the arguments, we would break code where the cont came from somewhere else
                       (read from memory, passed as a param, etc) and our simple renaming operation
                       with the parameter of manipK would not be sufficient. We would need to throughly
                       change the types of all such unknown cont vars in the program.
                    *)
                    val _ = if L.length params > 1 then
                                raise Fail ("escape cont " ^ (CV.toString f) ^ " takes more than 1 parameter!")
                            else ST.tick cntExpand

                    (* val _ = print ("Wrapping cont " ^ (CV.toString f) ^ "\n") *)

                    val retk = getRet env
                    val (padFB as C.FB{f=retkWrap,...}) = mkLandingPad(retk, f)

                    fun mkManipKBody env (newF, newActiveRetk, manipKRetParam) = let
                        val env = insertV(env, retk, RetCont newActiveRetk)
                        val env = insertV(env, f, EscapeCont newF)
                        val env = setRet(env, newActiveRetk)
                        val env = setParamRet(env, manipKRetParam)
                        val env = setManipScope(env, true)
                    in
                        doExp(env, e)
                    end

                    val (manipFB as C.FB{f=manipK,...}) = mkManipFun(f, retk, mkManipKBody env)

                    val contBody = doExp(env, body)

                    (* set/update classifications *)
                    val _ = (K.setKind(retkWrap, K.ReturnCont) ; K.setKind(f, K.JoinCont))
                 in
                    C.Cont(C.FB{f=f,params=params,rets=rets, body=contBody},
                        C.mkCont(padFB,
                            C.mkFun([manipFB],
                                MK.dummyExh(fn unitExh =>
                                    C.mkCallec(manipK, [retkWrap, unitExh])))))
                 end

              | _ => C.Cont(C.FB{f=f,params=params,rets=rets, body = doExp(env, body)}, doExp(env, e))
             (* esac *))
        (* esac *))
    end

    and doFun env (C.FB{f, params, rets as (retk::_), body}) = let
        val env = setRet(env, retk)
        val env = setParamRet(env, retk)
        val env = setManipScope(env, false)
    in
        C.FB{f=f,params=params,rets=rets, body= doExp(env, body) }
    end



    and start moduleBody = doFun (emptyEnv()) moduleBody

    (* the tricky part here is that we need to merge the type of the arguments
       to these two continuations so we can longjmp or return to the same block.
       Of course, we do not know all throw sites of an escape cont, so we need a
       uniform calling convention.

       The approach we're taking is that if there is more than 1 arg,
       then it must be bundled up as a tuple. Once we enter the landing pad,
       we unbundle and throw.
    *)
    and mkLandingPad (retk, kont) = let

        val padTy = CPSTy.T_Cont([indicatorTy, CPSTy.T_Any])
        val padVar = CV.new("setjmpLandingPad", padTy)

        val boolParam = CV.new("regularRet", indicatorTy)
        val valParam = CV.new("arg", CPSTy.T_Any)

        fun branch(trueExp, falseExp) =
            MK.fresh(indicatorTy, fn fals =>
              C.mkLet([fals], C.Const(falseVal, indicatorTy),
                C.mkIf(Prim.I32NEq(boolParam, fals), trueExp, falseExp)))

        (* this should match up with MK.bundle !! *)
        fun dispatch cont = (case MK.argTysOf cont
            of [] => C.mkThrow(cont, []) (* weird but okay i guess *)

             | [ty] => MK.cast(valParam, ty, fn arg => C.mkThrow(cont, [arg]))

             | tys => MK.cast(valParam, CPSTy.T_Tuple(false, tys), fn tup =>
                        MK.selectAll(tys, tup, fn args =>
                          C.mkThrow(cont, args)))
             (* esac *))
    in
        C.FB{f = padVar, params = [boolParam, valParam], rets = [],
                body = branch(dispatch retk, dispatch kont)}
    end

    (* the reason manipK takes an exh that will be unused is because it will use
       the standard calling convention (its caller is actually an unknown fun). *)
    and mkManipFun(origLetCont, origRetk, k) = let
        val contAny = CPSTy.T_Cont([CPSTy.T_Any])
        val retkTy = CPSTy.T_Cont([indicatorTy, CPSTy.T_Any])

        val contP = CV.copy origLetCont
        val retkP = CV.new("landingPadK", retkTy)
        val exnP = CV.new("deadExnK", contAny)

        val num = Int.toString(ST.count cntExpand) (* aids debugging *)
        val fname = CV.new("manipK" ^ num, CPSTy.T_Fun([CV.typeOf contP], [retkTy, contAny]))

        (* build the invoke return cont *)
        val invokeParamTys = L.map (fn _ => CPSTy.T_Any) (MK.argTysOf origRetk)
        val invokeRet = CV.new("invokeRetk", CPSTy.T_Cont invokeParamTys)
        val invokeParams = L.map (fn ty => CV.new("param", ty)) invokeParamTys

        (* set kind. also need to set contexts since this is a ParamCont *)
        val _ = (K.setKind(invokeRet, K.JoinCont) ;
                 K.setContextOfThrow(retkP, VSet.singleton fname) ;
                 K.setContextOfThrow(contP, VSet.singleton fname))

                 (* TODO: more accurately:
                        contextsOf(contP) :=
                            if enclosingFunOf(origLetCont) in contextsOf(origLetCont)
                                then union({ fname },
                                        contextsOf(origLetCont) \ { enclosingFunOf(origLetCont) })
                                else contextsOf(origLetCont)

                    not really critical though since closure conversion doesn't care if
                    its a non-ret param. We just have to have at least one context that
                    tells closure conversion it's not a ret param.
                  *)

        fun mkInvokeRet k =
            C.mkCont(C.FB {
                f = invokeRet,
                params = invokeParams,
                rets = [],
                body = MK.bundle(invokeParams, fn newArgs =>
                         MK.atLeastOneArg(newArgs, fn paddedArgs =>
                           MK.bindTrue(fn tru =>
                             C.mkThrow(retkP, tru::paddedArgs))))},
                k invokeRet)

    in
        C.FB {
            f = fname,
            params = [contP],
            rets = [retkP, exnP],
            body = mkInvokeRet(fn invokeRet => k (contP, invokeRet, retkP))
        }
    end

    (* ClassifyConts must be run before this transform. *)
    fun transform (m as C.MODULE{name, externs, body}) =
        if not(Controls.get BasicControl.direct)
        then m
        else let
            val m = C.MODULE{name=name,externs=externs,body = start body }
            val _ = CPSCensus.census m
        in
            m
        end

  end
