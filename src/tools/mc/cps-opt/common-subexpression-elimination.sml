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
    val cseFlg = ref true
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

    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))


  (* apply a substitution to a list of variables *)
    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst(env, x) :: subst'(env, xs)

  (***** transform ******)

    (*
     * This transformation currently only tracks selections.
     * So, for any tuple variable x, it will record a list
     * of i*y where each i is an index and y is the name of
     * another variable bound to that index.
     * When there is a hit in that map, switch the uses of
     * x for y in the rest of the program. When there is
     * not, add an entry to the cseMap.
     *)
    fun doExp (env, cseMap, C.Exp(ppt, t)) = 
        (case t
	  of C.Let(lhs as [l], rhs as C.Select(i, x), e) => let
                 val x = subst(env, x)
             in
                 case VMap.find (cseMap, x)
                  of SOME x' =>
                     (case List.find (fn (x,_) => x=i) x'
                       of SOME (_, l') => 
                          (case CV.typeOf l'
                            of CPSTy.T_Tuple(_,_) => (
                               ST.tick cntElim;
                               doExp(VMap.insert(env, l, l'), cseMap, e))
                             | _ => (* This error occurs when we have added a variable that is selected
                                      * into elsewhere, but that variable does not actually have a tuple
                                      * type.*)
                               C.mkLet(lhs, C.Select(i, x), doExp(env, cseMap, e)))
                        | NONE => let
                              val cseMap = VMap.insert (cseMap, x, (i, l)::x')
                          in
                              C.mkLet(lhs, C.Select(i, x), doExp(env, cseMap, e))
                          end)
                   | NONE => let
                         val cseMap = VMap.insert (cseMap, x, [(i, l)])
                     in
                         C.mkLet(lhs, C.Select(i, x), doExp(env, cseMap, e))
                     end
             end
           | C.Let(lhs, rhs, e) => C.mkLet(lhs, CPSUtil.mapRHS (fn (x) => subst (env, x)) rhs, doExp(env, cseMap, e))
	   | C.Fun(fbs, e) =>
             C.mkFun(List.map (fn (x) => doFB (env, cseMap, x)) fbs, doExp(env, cseMap, e))
	   | C.Cont(fb, e) => 
             C.mkCont(doFB (env, cseMap, fb), doExp(env, cseMap, e))
	   | C.If(x, e1, e2) => C.mkIf(CondUtil.map (fn (v) => subst (env, v)) x, doExp(env, cseMap, e1), doExp(env, cseMap, e2))
	   | C.Switch(x, cases, dflt) =>
             C.mkSwitch(
	     subst(env, x),
	     List.map (fn (tag, e) => (tag, doExp(env, cseMap, e))) cases,
	     Option.map (fn e => doExp (env, cseMap, e)) dflt)
	   | C.Apply(f, args, rets) => C.mkApply(subst(env, f), subst'(env, args), subst'(env, rets))
	   | C.Throw(k, args) => C.mkThrow(subst(env, k), subst'(env, args)))
    and doFB (env, cseMap, C.FB{f, params, rets, body}) =
        C.FB{f=f, params=params, rets=rets, body=doExp (env, cseMap, body)}

    fun transform (m as C.MODULE{name, externs, body}) =
	  if !cseFlg
	    then let
	      val body = doFB (VMap.empty, VMap.empty, body)
              val m' = C.MODULE{name=name, externs=externs, body=C.mkLambda body}
              val _ = Census.census m'
	      in
                  m'
	      end
	    else m

  end

