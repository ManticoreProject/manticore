(* copy-propagation.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

structure FunctionHoisting : sig

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
              
    val reorderDebug = ref false
    val hoistFlag = ref true
    val () = List.app (fn ctl => ControlRegistry.register CPSOptControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = reorderDebug,
                  name = "fun-reorder-debug",
                  pri = [0, 1],
                  obscurity = 1,
                  help = "enable function reordering debug"
                },
              Controls.control {
                  ctl = hoistFlag,
                  name = "enable-fun-reorder",
                  pri = [0, 1],
                  obscurity = 1,
                  help = "enable function reordering"
                }
            ]          
              
    (* property for tracking bindings to function bodies*)
    local
      val {setFn, getFn, ...} = CV.newProp (fn f => raise Fail "Undefined function binding during function hoisting.")
    in
    fun setFB (f,b : C.lambda) = setFn (f, b)
    fun getFB f = getFn f
    end          
              
              
    fun getCallers f = 
    let exception UnknownCaller
        fun getCallers(C.FB{f, body,...}) = case CFA.callersOf f 
                of CFA.Known(s) => VSet.union(s, getExpCallers body)
                  |CFA.Unknown => raise UnknownCaller
        and getExpCallers(C.Exp(ppt, t)) = case t 
             of C.Let(_, _, e) => getExpCallers e
               |C.Fun(lambdas, body) => 
                     let val s1 = List.foldl (fn (fb, s) => VSet.union(s, getCallers fb)) VSet.empty lambdas
                     in VSet.union(getExpCallers body, s1)
                     end
               |C.Cont(lambda, body) => 
                     let val s1 = getCallers lambda
                     in VSet.union(getExpCallers body, s1)
                     end
              |C.If(_, e1, e2) => VSet.union(getExpCallers e1, getExpCallers e2)
              |C.Switch(v, cases, body) => 
                    let val s1 = List.foldl (fn ((tag, e), s) => VSet.union(s, getExpCallers e)) VSet.empty cases
                    in case body of
                           SOME(b) => VSet.union(getExpCallers b, s1)
                          |NONE => s1
                    end
              |C.Apply _ => VSet.empty
              |C.Throw _ => VSet.empty
    in getCallers f handle UnknownCaller => VSet.empty      
    end
             
             
    fun insert (map, callee, callers : VSet.set) = 
            case VMap.find (map, callee)
             of NONE => VMap.insert (map, callee, callers)
              | _ => map
    
    (*Map a function f to all those that call f from above in the program*)
    fun mapToCallers(fb as C.FB{f,...}, env, map) = 
        let val callers = getCallers fb
            val aboveCallers = VSet.filter (fn x => VSet.member(env, x)) callers
            val nonRecursiveCallers = VSet.filter (fn f' => not(CV.same(f, f'))) aboveCallers
            val _ = if !reorderDebug
                    then (print ("Mapping " ^ CV.toString f ^ " to: " ^ String.concatWith ", " (List.map (fn x => CV.toString x) (VSet.listItems nonRecursiveCallers)) ^ "\n"); print (CV.toString f ^ "'s callers are: " ^ String.concatWith ", " (List.map (fn x => CV.toString x) (VSet.listItems callers)) ^ "\n") )
                    else ()     
        in insert(map, f, nonRecursiveCallers)
        end         
                      
          
    (*Create a mapping from function names to all of the function bindings that should be moved
    **to a position preceeding them in the program.  env is a set containing all functions seen so far*)      
    fun analyze (body, env, theMap) = 
        let fun doExp (C.Exp(ppt, e), env, map) = case e
                of C.Let(vars, rhs, e) => doExp(e, env, map)
                  |C.Fun(lambdas, body) => 
                        let val (_, map') = List.foldr (fn (fb,  (_, map)) => doLambda(fb, env, map, length lambdas > 1))
                                                        (env, map) lambdas
                            val env' = List.foldr (fn (C.FB{f,...}, env) => VSet.add(env, f)) env lambdas                      
                        in doExp(body, env', map')
                        end
                  |C.Cont(lambda, body) => 
                        let val (env', map') = doLambda(lambda, env, map, false)
                        in doExp (body, env', map')
                        end
                  |C.If(v, e1, e2) => 
                        let val (_, map') = doExp(e1, env, map)
                            val (_, map'') = doExp(e2, env, map')
                        in (env, map'')
                        end
                  |C.Switch (v, cases, body) => 
                        let val (_, map') = List.foldl (fn ((tag, e), (env, map)) => 
                                                doExp(e, env, map)) (env, map) cases
                            val (_, map'') = case body of
                                SOME(x) => doExp(x, env, map')
                               |NONE => (env, map')     
                               (*end case*)
                            in (env, map'')                  
                            end
                  |C.Apply (f, args, rets) => (env, map)
                  |C.Throw (f, args) => (env, map)      
            and doLambda (fb as C.FB{f,params,rets,body}, env, map, mutRec) = 
                let val _ = setFB(f, fb)
                    val env' = VSet.add(env, f)
                    (*If there are no free vars, then map this function to all its callers*)
                    val fvs = FreeVars.envOfFun f
                    val _ = if !reorderDebug
                            then if VSet.isEmpty fvs
                                 then print (CV.toString f ^ " is closed\n")
                                 else print (CV.toString f ^ " is not closed\n")
                            else ()
                    val map' = if VSet.isEmpty fvs andalso not(mutRec) 
                               then mapToCallers(fb, env', map) 
                               else map
                in doExp(body, env', map')
                end
        in doLambda (body, env, theMap, false)
        end          
              
    fun getFbName(C.FB{f,...}) = f     
              
    fun reorderFuns(C.MODULE{name,externs,
                                body=(mainLambda as C.FB{f=main,params,rets,body=modBody})}) =
    let val externsEnv = List.foldl
		             (fn (cf, env) => VSet.add(env, CFunctions.varOf cf))
		             VSet.empty externs
		             
	fun wrapWithNewPreds (lambdas, hoisted, map, continue) =
                let fun wrapWithNewPreds' (l'::rest, wrapper, hoisted, map, continue) = let
                        val C.FB{f=f', ...} = l'
                        (* Extract all of the functions that are supposed to be moved to a point in 
                        **the program before this one *)
                        val preds = List.map (fn (k,v) => getFB k)
                                             (VMap.listItemsi
                                                  (VMap.filter
                                                       (fn a => VSet.member(a, f')) map))       
                                                       
                        val map = List.foldr (fn (key, map) => #1(VMap.remove(map, key))) map (List.map getFbName preds)   
                        (*Add preds to the list of functions being hoisted*)
                        val hoisted = VSet.addList(hoisted, (List.map getFbName preds))                  
                        fun wrapFun ((p as C.FB{f, rets,...})::preds, wrapper, map) =
                                let val _ = if !reorderDebug
                                            then print (concat [CV.toString f, " is being hoisted above ",
                                                                CV.toString f', ".\n"])
                                            else ()
                                    val ty = CV.typeOf f
                                    val isCont = case ty of 
                                                        CTy.T_Cont _ => true
                                                       |_ => false        
                                in
                                    if isCont
                                    then wrapFun (preds, (fn x => C.mkCont (p, x)) o wrapper,
                                                       map)
                                    else wrapFun (preds, (fn x => C.mkFun ([p], x)) o wrapper,
                                                          map)
                                end
                          | wrapFun ([], wrapper, map) = wrapWithNewPreds' (rest, wrapper, hoisted, map, continue)
                    in
                        wrapFun (preds, wrapper, map)
                    end
                      | wrapWithNewPreds' ([], wrapper, hoisted, map, continue) = continue (wrapper, hoisted, map)
                in
                    wrapWithNewPreds' (lambdas, (fn x => x), hoisted, map, continue)
                end	             
		             
        and doExp (exp as C.Exp(ppt, e), hoisted, map) = 
            case e
             of C.Let (vars, rhs, e) => let
                    val (body, hoisted, map) = doExp (e, hoisted, map)
                in (C.mkLet (vars, rhs, body), hoisted, map) 
                end
              | C.Fun (lambdas, body) => 
                wrapWithNewPreds(List.filter (fn C.FB{f,...} => not(VSet.member(hoisted, f))) lambdas, hoisted, map, fn (wrapper, hoisted, map) => 
                                (*Remove any lambdas that were already hoisted*)
                        let val lambdas = List.filter (fn C.FB{f,...} => not(VSet.member(hoisted, f))) lambdas
                            val (lambdas', hoisted', map') = List.foldr (fn (C.FB{f, params, rets, body}, (fbs, hoisted, map)) => 
                                let val _ = print("Calling doExp1 with " ^ VSet.foldl(fn(f, s) => CV.toString f ^ ", " ^ s) "" hoisted ^ "\n")
                                    val (body', hoisted', map') = doExp(body, hoisted, map)
                                in (C.FB{f=f, params=params, rets=rets, body=body'}::fbs, hoisted', map')
                                end) ([], hoisted, map) lambdas
                            val lambdas'' = List.rev lambdas'
                            val _ = print ("calling doexp2 with " ^ VSet.foldl(fn(f, s) => CV.toString f ^ ", " ^ s) "" hoisted' ^ "\n")
                            val (body', hoisted'', map'') = doExp(body, hoisted', map)
                        in if List.null lambdas''
                           then (wrapper body', hoisted'', map'')
                           else (wrapper(C.mkFun(lambdas'', body')), hoisted'', map'')
                        end)
              | C.Cont(lambda as C.FB{f, params, rets, body=cBody}, body) => 
                (print ("C.cont case with " ^ VSet.foldl(fn(f, s) => CV.toString f ^ ", " ^ s) "" hoisted ^ "\n");
                wrapWithNewPreds([lambda], hoisted, map, fn (wrapper, hoisted, map) => 
                                let val (cBody', hoisted', map') = doExp(cBody, hoisted, map)
                                    val lambda = C.FB{f=f,params=params, rets=rets, body=cBody'}
                                    val (body', hoisted'', map'') = doExp(body, hoisted', map')
                                in if VSet.member(hoisted', f)
                                   then (wrapper body', hoisted'', map'')
                                   else (wrapper(C.mkCont(lambda, body')), hoisted'', map'')
                                end))
              | C.If (v, e1, e2) => let
                    val (e1', hoisted', map') = doExp (e1, hoisted, map)
                    val (e2', hoisted'', map'') = doExp (e2, hoisted', map')
                in
                    (C.mkIf (v, e1', e2'), hoisted'', map'')
                end
              | C.Switch (v, cases, body) => let
                    val (switches, hoisted', map') = List.foldr (fn ((tag, e), (rr, hoisted, map)) => let
                                                              val (body, hoisted', map') = doExp (e, hoisted, map)
                                                          in ((tag, body)::rr, hoisted', map') end) ([], hoisted, map) cases
                    val (default, hoisted'', map'') = (case body
                                               of SOME(x) => let
                                                      val (e, hoisted'', map'') = doExp (x, hoisted', map')
                                                  in (SOME(e), hoisted'', map'') end
                                                | NONE => (NONE,  hoisted', map'))
                in
                    (C.mkSwitch(v, switches, default), hoisted'', map'')
                end
              | C.Apply (f, args, retArgs) => (exp, hoisted, map)
              | C.Throw (k, args) => (exp, hoisted, map)
        (* end case *)
        val env = VSet.add (externsEnv, main)
        
        val (_, map) = analyze(mainLambda, env, VMap.empty)
        val (body', _, _) = doExp(modBody, VSet.empty, map)
    in
        C.MODULE{
	name=name, externs=externs,
	body = C.mkLambda(C.FB{
                          f=main,params=params,rets=rets,
                          body=body'
		         }, false)
	}
    end

    fun dump m prefix = 
        let val file = TextIO.openOut(prefix ^ "liftingOutput.cps")
        in PrintCPS.output(file, m)
        end

    fun transform m = 
        let val _ = dump m "pre"
            val _ = FreeVars.clear m
            val _ = FreeVars.analyze m
            val m' = reorderFuns m
            val _ = dump m' "post"
        in m'
        end









  end
