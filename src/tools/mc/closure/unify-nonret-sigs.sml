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
 *  FIXME add ability to drop the return cont of a fun so we can avoid this.
 *)

structure UnifyNonRetSigs : sig

    val transform : CPS.module -> CPS.module

  end = struct

  structure C = CPS
  structure CV = C.Var
  structure VMap = CV.Map
  structure ST = Stats
  structure Census = CPSCensus

  (* look at Apply, and if the callee's signature will be changed,
     then replace the retk with unit of type 'cont(any)'.
     *)
  fun doExp (env, e) = e


  (* if useCnt f == appCnt f
       and useCnt retK == 0
       and not (match (typeOf(retK), 'cont(any)'))

       then replace retK param with one of the new type 'cont(any)'.
            no updates are needed on the expressions in its scope
            since it's unused anyway.

      else keep going

  *)
  fun doFB (env, C.FB{f, params, rets, body}) =
    C.FB{f=f, params=params, rets=rets, body= doExp (env, body) }


  fun transform (m as C.MODULE{name, externs, body}) =
      C.MODULE {name=name, externs=externs, body = doFB ([], body)}

  end
