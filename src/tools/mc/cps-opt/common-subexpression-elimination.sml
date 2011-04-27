(* common-subexpression-elimination.sml
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure CommonSubexpressionElimination : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure ST = Stats
    structure Census = CPSCensus


  (***** controls ******)
    val cseFlg = ref false
    val cseDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = cseFlg,
                  name = "cse",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable common-subexpression-elimination"
                },
              Controls.control {
                  ctl = cseDebug,
                  name = "cse-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug common-subexpression-elimination"
                }
            ]


  (********** Counters for statistics **********)

    val cntElim	= ST.newCounter "cse:elim"


  (***** var to var substitution ******)

    type env = C.var VMap.map

    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))


  (***** transform ******)

    fun doExp (env, C.Exp(ppt, t)) = 
        (case t
	  of C.Let(lhs, rhs, e) => C.mkLet(lhs, rhs, doExp(env, e))
	   | C.Fun(fbs, e) =>
             C.mkFun(List.map (fn (x) => doFB (env,x)) fbs, doExp(env, e))
	   | C.Cont(fb, e) => 
             C.mkCont(doFB (env, fb), doExp(env, e))
	   | C.If(x, e1, e2) => C.mkIf(x, doExp(env, e1), doExp(env, e2))
	   | C.Switch(x, cases, dflt) =>
             C.mkSwitch(
	     x,
	     List.map (fn (tag, e) => (tag, doExp(env, e))) cases,
	     Option.map (fn e => doExp (env, e)) dflt)
	   | C.Apply(f, args, rets) => C.mkApply(subst(env, f), args, rets)
	   | C.Throw(k, args) => C.mkThrow(subst(env, k), args))
    and doFB (env, C.FB{f, params, rets, body}) =
        C.FB{f=f, params=params, rets=rets, body=doExp (env, body)}

    fun transform (m as C.MODULE{name, externs, body}) =
	  if !cseFlg
	    then let
	      val body = doFB (VMap.empty, body)
              val m' = C.MODULE{name=name, externs=externs, body=C.mkLambda body}
              val _ = Census.census m'
	      in
                  m'
	      end
	    else m

  end

