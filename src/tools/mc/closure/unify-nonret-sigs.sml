(* unify-nonret-sigs.sml
 *
 * COPYRIGHT (c) 2018 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This pass only exists because we currently don't have the ability to
 * say that a function has no return continuation. That is, some cont variable,
 * even without any uses, must appear in the list of `rets` and correctly
 * typecheck with the rest of the program.
 *
 * This causes trouble with the interaction between elim-uncalled and
 * wrap-captures when the wrapped expression within an escape continuation
 * contains a call to a function that does not return normally.
 *
 * Thus, this pass matches up the return types of all functions with
 * unused return continuations so that we can resolve this mismatch
 * in wrap-captures.
 *
 *  FIXME add ability to drop the return cont of a fun so we can
 *        eliminate the need for this pass.
 *)

structure UnifyNonRetSigs : sig

    val transform : CPS.module -> CPS.module

    val tgtType : CPSTy.ty

  end = struct

  structure C = CPS
  structure CV = C.Var
  structure CT = CPSTy
  structure VSet = CV.Set
  structure ST = Stats
  structure Census = CPSCensus

  (* Utilities *)

  val tgtType = CPSTy.T_Cont([CPSTy.T_Any])
  val useCnt = CV.useCount
  val appCnt = CV.appCntOf
  fun decUse v = CV.addToCount (v, ~1)
  fun incUse v = CV.addToCount (v, 1)

  (* Determine whether to change the retk of this fb.
     the env will have the fb added to it if change is needed *)
  fun inspectFB (fb as C.FB{f, rets, ...}, env) = let
      val SOME retK = CPSUtil.getRetK fb
      val retTy = CV.typeOf retK
    in
      if useCnt f = appCnt f andalso
         useCnt retK = 0     andalso
         not (CPSTyUtil.match (retTy, tgtType))
      then VSet.add (env, f)
      else env
    end

  (* use count should be 0, so we just update the types *)
  fun fixup (x as (f, retk::_)) =  let
      val (argTys, _::rest) = CPSTyUtil.asFunTy (CV.typeOf f)
    in
      (CV.setType (retk, tgtType) ;
       CV.setType (f, CT.T_Fun(argTys, tgtType::rest)) ;
       x)
    end

    fun unitRetk (ty, k) = let
          val retk = CV.new("unitRetk", CPSTy.T_Any)
      in
          C.mkLet([retk], C.Const(Literal.unitLit, CPSTy.T_Any),
            cast(retk, ty, k))
      end
    and cast (var, targTy, k) = let
        val lhs = CV.new(CV.nameOf var, targTy)
    in
        (incUse var ; C.mkLet([lhs], C.Cast(targTy, var), k lhs))
    end


  (* Main transform functions *)

  fun doFB (env, C.FB{f, params, rets, body}) = let
      val (f, rets) = if VSet.member (env, f)
                        then fixup (f, rets)
                        else (f, rets)
    in
      C.FB{ f = f, params = params,
            rets = rets,
            body = doExp (env, body) }
    end

  and doExp (env, C.Exp (ppt, term)) = let
        fun wrap term = C.Exp (ppt, term)
    in case term
    of C.Let (vs, rhs, e) => wrap (C.Let (vs, rhs, doExp (env, e)))
     | C.Cont (lam, e)    => wrap (C.Cont (doFB (env, lam), doExp (env, e)))
     | C.If (cnd, e1, e2) => wrap (C.If (cnd, doExp(env, e1), doExp(env, e2)))
     | C.Switch (v, alts, dflt) =>
            wrap (C.Switch (v,
                      map (fn (t, e) => (t, doExp(env, e))) alts,
                      Option.map (fn e => doExp (env, e)) dflt
                      ))
     | C.Callec _ => wrap term
     | C.Throw _  => wrap term

     (* cases where we actually do something interesting *)

     | C.Apply (f, args, old :: rest) =>
        if not (VSet.member (env, f))
        then wrap term
        else unitRetk (tgtType, fn retK => (
                decUse old ; incUse retK ;
                wrap (C.Apply (f, args, retK :: rest))))


     | C.Fun (lams, e) => let
          val newEnv = foldl inspectFB env lams
          val newLams = map (fn lam => doFB (newEnv, lam)) lams
        in
          wrap (C.Fun (newLams, doExp (newEnv, e)))
        end
    (* end case *)
    end


  fun transform (m as C.MODULE{name, externs, body}) =
      C.MODULE {name=name, externs=externs, body = doFB (VSet.empty, body)}

  end
