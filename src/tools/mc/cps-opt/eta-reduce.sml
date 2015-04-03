(* red-reduce.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * for each definition of the form:
 *      - fun f x = f' x 
 *      - cont k x = throw k' x
 * rewrite all uses as f'/k' respectively
 *)

structure EtaReduce : sig

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
    structure Census = CPSCensus
    structure PSet = PPt.Set
    structure CU = CPSUtil
    
  (***** controls ******)
    val enableEtaReduction = ref true
    val etaReduceDebug = ref false

    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = enableEtaReduction,
                  name = "eta-reduction",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable eta reduction"
                },
              Controls.control {
                  ctl = etaReduceDebug,
                  name = "eta-reduction-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug eta reduction"
                  }
            ]

    (*is this a function or a continuation*)
    datatype functionSort = Fun | Cont

    fun reducable(C.Exp(_, t), args: C.var list, rets : C.var list, sort : functionSort) = 
        let fun argsSame x =
                    case x
                        of (a1::args1, a2::args2) => CV.same(a1, a2) andalso argsSame(args1, args2)
                         | (nil, nil) => true
                         | _ => false 
        in case (t, sort)
            of (C.Apply(f', args', rets'), Fun) =>
                   if argsSame(args, args') andalso argsSame(rets, rets')
                   then SOME f'
                   else NONE
            | (C.Throw(f', args'), Cont) => if argsSame(args, args') then SOME f' else NONE
            | _ => NONE
        end

    (* functions to update census counts *)
    fun inc x = CV.addToCount(x, 1)
    fun dec x = CV.addToCount(x, ~1)
    
    (* apply a substitution to a variable *)
    fun subst env x = case VMap.find(env, x)
	   of SOME y => (inc y; y)
	    | NONE => x

  (* apply a substitution to a list of variables *)
    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst env x :: subst'(env, xs)
            
    fun etaReduce(C.MODULE{name,externs,body=mainLambda as C.FB{f=main,params,rets,body=modBody}}) = 
        let fun doRHS(s, rhs) = 
                case rhs
                    of C.Var vs => C.Var (subst'(s, vs))
                     | C.Cast(ty, x) => C.Cast(ty, subst s x)
                     | C.Const _ => rhs
                     | C.Select(i, x) => C.Select(i, subst s x)
                     | C.Update(i, x, y) => C.Update(i, subst s x, subst s y)
	                 | C.AddrOf(i, x) => C.AddrOf(i, subst s x)
	                 | C.Alloc(ty, args) => C.Alloc(ty, subst'(s, args))
	                 | C.Promote x => C.Promote(subst s x)
	                 | C.Prim p => C.Prim(PrimUtil.map (subst s) p)
	                 | C.CCall(f, args) => C.CCall(subst s f, subst'(s, args))
	                 | C.HostVProc => rhs
	                 | C.VPLoad(n, x) => C.VPLoad(n, subst s x)
	                 | C.VPStore(n, x, y) => C.VPStore(n, subst s x, subst s y)
	                 | C.VPAddr(n, x) => C.VPAddr(n, subst s x)
            fun doLambda (l as C.FB{f=f,params=params,rets=rets,body=body}, (subst, ls)) = 
                case reducable(body, params, rets, Fun)
                    of SOME f' =>
                        let val subst = CV.Map.insert(subst, f, f')
                            val _ = dec f
                        in (subst, ls)
                        end
                     | None => 
                        let val body' = CU.substExp(subst, body)
                        in (subst, C.FB{f=f,params=params,rets=rets,body=doExp(subst, body')}::ls)
                        end
            and doLambdas(s, lambdas, e) = 
                let val (s', lambdas) = List.foldl doLambda (s, nil) lambdas
                in C.mkFun(List.rev lambdas, doExp(s', e)) end
            and doCont (s, cont as C.FB{f=f,params=params,rets=rets,body=body}, e) = 
                case reducable(body, params, rets, Cont)
                   of SOME f' => 
                        let val subst = CV.Map.insert(s, f, f')
                            val _ = dec f
                        in doExp(subst, e)
                        end
                    | NONE => C.mkCont(C.FB{f=f,params=params,rets=rets,body=doExp(s, body)}, doExp(s, e))
            and doExp (s, C.Exp(pt, t)) = 
                case t
                    of C.Let(lhs, rhs, e') => C.mkLet(subst'(s, lhs), doRHS(s, rhs), doExp(s, e'))
                     | C.Fun(lambdas, e') => doLambdas(s, lambdas, e')
                     | C.Cont(cont, e') => doCont(s, cont, e')
                     | C.If(cond, tBranch, fBranch) => C.mkIf(cond, doExp(s, tBranch), doExp(s,fBranch))
                     | C.Switch(v, branches, opt) => 
                        let val branches' = List.map (fn (t,e) => (t, doExp(s, e))) branches
                            val opt' = Option.map (fn e => doExp(s, e)) opt
                        in C.mkSwitch(v, branches', opt')
                        end
                     | C.Apply(f, args, rets) => 
                        let val f' = subst s f
                            val args' = subst'(s, args)
                            val rets' = subst'(s, rets)
                        in C.mkApply(f', args', rets') end
                     | C.Throw(k, args) =>
                        let val k' = subst s k
                            val args' = subst'(s, args)
                        in C.mkThrow(k', args') end
            val body' = doExp(CV.Map.empty, modBody)
        in C.MODULE{name=name,externs=externs,
                    body=C.mkLambda(C.FB{f=main,params=params,rets=rets,body=body'}, false)}
        end

    fun transform m =
        if !enableEtaReduction
	    then let val m = etaReduce m
	             val _ = Census.census m
	         in m end
        else m

  end



















  
