(* wrap-captures.sml
 *
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * After cont classification for direct-style conversion, we wrap continuation
 * captures so they may be emitted correctly by the code generator. In particular,
 * we perform the following type of expansion in order to turn the definition
 * of an escape continuation into one where the binding location of all
 * uses for that continuation is the parameter of a function that is invoked via
 * callec. Then, the original definition turns into a local continuation of
 * the enclosing function.
 *
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
 *      cont landingPad (arg : any, indicator : int) =    <-- a ret cont
 *       if indicator = regularRet
 *         then throw retK (#1(arg), #2(arg), ...)
 *       else if indicator = escapeThrow
 *         then throw k arg
 *       else if indicator = localThrow1
 *         then throw localCont arg
 *       ...
 *
 *      in
 *        fun manipK (k' : cont(t) / landingPad' : cont(any, int), deadExh) =
 *          cont invokeRetK (a1, a2, ...) =                  <-- a join/ret cont
                let x = alloc(a1, a2, ...)                  <-- only if more than 1 arg
 *              throw landingPad' (x, regularRet)
 *          in
 *          cont invokeLocalK (...) = ... throw landingPad' (..., localThrow1) in
 *            [ C / k -> k', retK -> invokeRetK, localK -> invokeLocalK ]
 *        (* end manipK *)
 *        callec ( manipK / landingPad, deadExh)
 *
 *  The callec construct encapsulates the runtime system manipulation of the
 *  current continuation in accordance to the type of call stack in use.
 *  Callec will allocate the following continuation to represent 'k':
 *
 *  - [ ASM_Resume (1 or 2) | stackPtr (@landingPad) | stackInfo ]
 *    When invoked, this little continuation simply passes the arg its given
 *    to the stack frame pointed to, and sets the appropriate arg register
 *    to indicate where control should resume.
 *
 *  - During translation, within the expression C, any throws must be updated
 *    match the new calling convention. We introduce local cont bindings at the
 *    start of the manip function to simplify this process, since we need to add
 *    an additional fixed argument.
 *
 *
 *
 *)

structure WrapCaptures : sig

    val transform : CPS.module -> CPS.module
    val correctlyWrapped : CPS.module -> bool

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure CTU = CPSTyUtil
    structure VMap = CV.Map
    structure VSet = CV.Set
    structure ST = Stats
    structure L = List
    structure LP = ListPair
    structure K = ClassifyConts
    structure FV = FreeVars

    (* +DEBUG *)
        fun prSet s = (
              print "{";
              VSet.foldl
                (fn (x, false) => (print("," ^ CV.toString x); false)
                  | (x, true) => (print(CV.toString x); false)
                ) true s;
              print "}")
    (* -DEBUG*)

    (********** Counters for statistics **********)
    val cntExpand = ST.newCounter "wrap-captures:expand"

    (***** environment utils *****)
    datatype environment = E of { sub : cont_kind VMap.map, (* cont substitution map. *)
                                  allLocals : VSet.set (* set of all locally bound conts in the current fun. *)
                                }
        and cont_kind = LocalCont of CV.var
                      | EscapeCont of CV.var

    fun emptyEnv () = E{ sub = VMap.empty,
                         allLocals = VSet.empty
                       }

    (* utilities for handling local cont set *)
    fun addLocalCont ((E{sub, allLocals}), k) =
        E{sub=sub, allLocals = VSet.add(allLocals, k) }

    fun addLocalConts ((E{sub, allLocals}), ks) =
        E{sub=sub, allLocals = VSet.addList(allLocals, ks) }

    fun resetLocalConts (E{sub, allLocals = dead}) =
        E{sub=sub, allLocals = VSet.empty }

    fun localConts (E{allLocals,...}) = allLocals

    (* finds the latest substitution in the env *)
    fun lookupKind (E{sub,...}, v) = let
        fun some tycon v = SOME(tycon v)
        fun get (tycon, v) = (case VMap.find(sub, v)
            of SOME(LocalCont newV) => get (some LocalCont, newV)
             | SOME(EscapeCont newV) => get (some EscapeCont, newV)
             | NONE => tycon v
            (* esac *))
    in
        get (fn _ => NONE, v)
    end


    (* finds the latest substitution in the env *)
    and lookupV(env as E{sub,...}, v) = (case VMap.find(sub, v)
        of SOME(LocalCont newV) => lookupV(env, newV)
         | SOME(EscapeCont newV) => lookupV(env, newV)
    | NONE => v)

    fun insertV (E{sub, allLocals}, var, valu) =
        E{ sub = VMap.insert(sub, var, valu), allLocals=allLocals }

    fun subst env v = lookupV(env, v)

    fun dumpEnv msg env = raise Fail "dumpEnv not implemented"

    (***** end of environment utils *****)


    val indicatorTy = CPSTy.T_Raw(RawTypes.T_Int)

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

        fun allocAll (vals, k) = let (* we need to box the arg *)
          val allocTy = CPSTy.T_Tuple(false, map CV.typeOf vals)
          val lhs = CV.new("box", allocTy)
        in
          C.mkLet([lhs], C.Alloc(allocTy, vals), k [lhs])
        end

        (* this should match up with 'unbundle' below .
           by bundle, we mean to bundle for passing through the
           landing pad. *)
        fun bundle (args, k) = (case args
          of [] => atLeastOneArg ([], k) (* we pass a dummy val. *)

           | [arg] => if CTU.isKind CPSTy.K_UNIFORM (CTU.kindOf (CV.typeOf arg))
                      (* we will not box it, just cast ty => any *)
                      then cast(arg, CPSTy.T_Any, fn newArg => k [newArg])
                      (* we must box it *)
                      else allocAll ([arg], k)

              (* All arity > 1 conts are boxed. *)
           | args => allocAll (args, k)
          (* end case *))

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

        fun bindInt (i, k) =
            fresh(indicatorTy, fn lhs =>
                C.mkLet([lhs], C.Const(Literal.Int i, indicatorTy),
                    k lhs))

        fun argTysOf cont = (case CV.typeOf cont
            of CPSTy.T_Cont tys => tys
             | _ => raise Fail "not a cont")

    end (* end MK *)

    (****** END OF helper funs to build expressions ******)

    fun start moduleBody = doFun (emptyEnv()) moduleBody

    and doExp (env, C.Exp(ppt, t)) = let
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
         | C.Apply (f, args, oldRetK :: rest) => let
              (* an Apply may change from being in tail position
                 to non-tail due to the landing pad capture. *)
              val newRetK = lookupV (env, oldRetK)
              val applyExp = wrap (C.Apply(f, L.map (subst env) args, newRetK :: rest))
              in
                if CV.same (newRetK, oldRetK)
                  then applyExp
                  else (K.setTailApply(applyExp, false) ; applyExp)
              end

         | C.Throw (k, args) => let
              val freshArgs = L.map (subst env) args
            in
              case lookupKind (env, k)
                of NONE => wrap(C.Throw(k, freshArgs))
                 | SOME (LocalCont newK) => wrap(C.Throw(newK, freshArgs))
                 | SOME (EscapeCont newK) =>
                      (* because we cannot have a stub to handle the calling
                         convention for us on throws to parameter-bound escape
                         conts, we need to do it at each throw. *)
                      MK.bundle(freshArgs, fn freshArgs =>
                        wrap(C.Throw(newK, freshArgs)))
            end

         | C.Cont (cont as (C.FB{f, params, rets, body}, e)) => wrap (
            case K.kindOfCont f
               of (K.GotoCont | K.OtherCont) =>
                   (* see NOTE [single-param cont] *)
                   if L.length params > 1
                     then raise Fail ("escape cont "
                                     ^ (CV.toString f)
                                     ^ " takes more than 1 parameter!")
                     else ( ST.tick cntExpand ;
                            landingPadCapture env cont )

                | _ => let  val env = addLocalCont(env, f)  in
                          C.Cont(C.FB{f=f,params=params,rets=rets,
                                body = doExp(env, body)}, doExp(env, e))
                      end
              (* end wrap *))
        (* esac *))
    end

    and doFun env (C.FB{f, params, rets as (retk::_), body}) = let
        val env = resetLocalConts env
        val env = addLocalCont (env, retk)
    in
        C.FB{f=f,params=params,rets=rets, body= doExp(env, body) }
    end


    (* Uses a "landing pad" to discern which continuation
      of this enclosing function that the new callee,
      a manipK wrapper function, would like to invoke.
      This is similar to setjmp/longjmp.  *)
    and landingPadCapture env (C.FB{f, params, rets, body}, e) = let

      val fvsOfRest = FV.freeVarsOfExp e

      val () = if Controls.get ClosureControls.debug
                 then (print(concat["WrapCaptures: FV of exp following ", CV.toString f, " = "]);
                      prSet fvsOfRest; print "\n")
                 else ()

      val (padFB as C.FB{f=padVar,...}, numbering, neverReturns)
                = mkLandingPad (env, FV.freeVarsOfExp e, f)

      val (manipFB as C.FB{f=manipK,...}) = mkManipFun (env, numbering, neverReturns, e)

      (* set/update classifications *)
      val _ = (K.setKind(padVar, K.ReturnCont) ; K.setKind(f, K.JoinCont))
    in
      C.Cont(C.FB{f=f,params=params,rets=rets, body = doExp(env, body)},
          C.mkCont(padFB,
              C.mkFun([manipFB],
                  MK.dummyExh(fn unitExh =>
                      C.mkCallec(manipK, [padVar, unitExh])))))
    end

    and mkLandingPad (env, fvs, contVar) = let
      (* determine which local continuations need to be included
         in the pad's dispatching code and assign a numbering where
         contVar is always 0 *)
      val updatedFVs = VSet.map (subst env) fvs
      val reqLocals = VSet.intersection (updatedFVs, localConts env)
      val items = contVar :: VSet.listItems reqLocals
      val enums = L.tabulate(length items, fn i => IntInf.fromInt i)
      val numbering = LP.zipEq (enums, items)

      (* NOTE this is not an optimization. For an Apply
         in tail position that does not return normally, but passes
         along an escape cont, we must ensure that the enclosing
         manipK function's return cont, aka the landing pad, has
         a type that is compatible with the originally enclosing
         parameter return continuation. *)
      val neverReturns = VSet.isEmpty reqLocals

      (* construct the landing pad *)
      val padTy = if neverReturns
                  then CPSTy.T_Cont([CPSTy.T_Any])
                  else CPSTy.T_Cont([CPSTy.T_Any, indicatorTy])

      val padVar = CV.new("setjmpLandingPad", padTy)

      val flagParam = CV.new("contFlag", indicatorTy)

      (* NOTE: always using a uniform type *)
      val valParam = CV.new("arg", CPSTy.T_Any)

      fun mkAlt (i, tgtK) = (Word.fromLargeInt i, unbundle tgtK)

      (* this calling convention must match up with "bundle" above *)
      and unbundle tgtK = (case MK.argTysOf tgtK
          of [] => C.mkThrow(tgtK, []) (* original cont takes no args *)

                      (* deal with our use of a box for 1 arg conts *)
           | [ty] => if CTU.isKind CPSTy.K_UNIFORM (CTU.kindOf ty)
                      (* we did not box it, just cast any => ty *)
                      then MK.cast(valParam, ty, fn arg =>
                              C.mkThrow(tgtK, [arg]))
                      (* we boxed it, so unbox it *)
                      else unboxThrow tgtK [ty]
              (* All arity > 1 conts were boxed. *)
           | tys => unboxThrow tgtK tys
           (* esac *))

      and unboxThrow tgtK tys =
          MK.cast(valParam, CPSTy.T_Tuple(false, tys), fn tup =>
            MK.selectAll(tys, tup, fn args =>
              C.mkThrow(tgtK, args)))


      in
        (if neverReturns
          then C.FB{f = padVar, params = [valParam], rets = [],
                    body = unbundle contVar
                   }
          else C.FB{f = padVar, params = [valParam, flagParam], rets = [],
                    body = C.mkSwitch(flagParam, map mkAlt numbering, NONE)
                   },
         numbering,
         neverReturns)
      end


    (* the reason manipK takes an exh that will be unused is because it will use
       the standard calling convention (its caller is actually an unknown fun). *)
    and mkManipFun (env, (_, escCont) :: others, neverReturns, expr) = let
        val contAny  = CPSTy.T_Cont([CPSTy.T_Any])

        val padTy = if neverReturns
                    then CPSTy.T_Cont([CPSTy.T_Any])
                    else CPSTy.T_Cont([CPSTy.T_Any, indicatorTy])

        (* create the parameter-passed cont var *)
        val contP = CV.copy escCont
        val _ = CV.setType (contP, contAny)
        val env = insertV(env, escCont, EscapeCont contP)

        val padK = CV.new("landingPadK", padTy)
        val exnP = CV.new("deadExnK", contAny)

        val num = Int.toString(ST.count cntExpand) (* aids debugging *)
        val fname = CV.new("manipK" ^ num, CPSTy.T_Fun([CV.typeOf contP], [padTy, contAny]))

        (* set kind. also need to set contexts since this is a ParamCont *)
        val _ = (K.setContextOfThrow(padK, VSet.singleton fname) ;
                 K.setContextOfThrow(contP, VSet.singleton fname))

        (* create stubs for the other conts so they pass the right flag. *)
        fun mkStubs (env, others, k) = let
            fun lp (env, []) = k env
              | lp (env, x::xs) =
                  mkStub(env, x, fn newEnv => lp (newEnv, xs))

            in
              lp (env, others)
            end

        (* creates stub continuations that know which index to use when
            returning to the caller so it ends up at the right continuation.
            Also sets up the environment for processing the body of the manip fun. *)
        and mkStub (env, (idx, tgtK), k) = let
            (* build the cont *)
            val paramTys = MK.argTysOf tgtK
            val newK = CV.new("gotoCaller", CPSTy.T_Cont paramTys)
            val params = map (fn ty => CV.new("param", ty)) paramTys

            (* setup new environment, with newK as a local cont within
               the manip function's body, and tgtK substituted for it. *)
            val env = insertV (env, tgtK, LocalCont newK)
            val env = addLocalCont (env, newK)
        in
        (  K.setKind(newK, K.JoinCont) ;
            C.mkCont(C.FB {
                f = newK,
                params = params,
                rets = [],
                body = MK.bundle(params, fn newArgs =>
                           MK.bindInt(idx, fn flag =>
                             C.mkThrow(padK, newArgs @ [flag])))},
                k (insertV (env, tgtK, LocalCont newK))
          ))
        end

    in
        C.FB {
            f = fname,
            params = [contP],
            rets = [padK, exnP],
            body = mkStubs (resetLocalConts env,  (* NOTE want a clean slate for expr *)
                            others,
                            fn newEnv => doExp(newEnv, expr))
        }
    end

    fun correctlyWrapped (m as C.MODULE{body,...}) = let
      (* NOTE: this is just a correctness check. Make sure CFA is up to date
         before making this query! *)
      val _ = ClassifyConts.clear m
      val _ = ClassifyConts.analyze m
      val C.FB{body,...} = body

      fun invalidKind (C.Exp(_, C.Cont(C.FB{f,...}, _))) =
        (case ClassifyConts.kindOfCont f
          of K.GotoCont => true
           | K.OtherCont => true
           | _ => false
          (* esac *))
        | invalidKind _ = false

    in
      not (CPSUtil.expExists invalidKind body)
    end

    (* UnifyNonRetSigs and ClassifyConts must be run before this transform. *)
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
