(* eff-sfs-closure.sml
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This transformation converts from a CPS IR with free variables to a
 * version of the CPS where closures are explicit and each function has
 * no free variables (known as CLO in the Shao/Appel work).
 *
 *       "Efficient and Safe-for-Space Closure Conversion
 *       Zhong Shao and Andrew W. Appel
 *       TOPLAS, V 22, Nr. 1, January 2000, pp 129-161.
 *
 * Note that we only use the portion of this work that determins which
 * FVs should be turned into parameters, based on the number of available
 * registers. Then, we rely on simple flat closure conversion to handle
 * both unknown functions (as in the SSCC work) and for known functions
 * that have too many FVs (different from the SSCC work, which uses linked
 * closures).
 *)

functor ClosureConvertFn (Target : TARGET_SPEC) : sig

    val transform : CPS.module -> CPS.module

  end = struct

    structure PPt = ProgPt
    structure C = CPS
    structure CV = C.Var
    structure VMap = CV.Map
    structure VSet = CV.Set
    structure U = CPSUtil
    structure CTy = CPSTy
    structure CFA = CFACPS
    structure ST = Stats

  (***** Statistics *****)
    val cntFunsClosed		= ST.newCounter "clos:num-funs-closed"
    val cntFunsPartial		= ST.newCounter "clos:num-funs-partially-closed"

  (***** controls ******)
    val enableClosureConversion = ref false
    val closureConversionDebug = ref true

    val () = List.app (fn ctl => ControlRegistry.register ClosureControls.registry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
              envName = NONE
            }) [
              Controls.control {
                  ctl = enableClosureConversion,
                  name = "closure-convert",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "enable Shao/Appel Eff/SfS closure conversion"
                },
              Controls.control {
                  ctl = closureConversionDebug,
                  name = "closure-convert-debug",
                  pri = [0, 1],
                  obscurity = 0,
                  help = "debug closure conversion "
                  }
            ]

    (* The stage number is:
     * 1 for the outermost function
     * 1+ SN(g) if if is a function and g encloses f
     * 1+ max{SN(g) | g \in CFA.callersOf k} if k is a continuation
     *)
    local
        val {setFn, getFn=getSN, ...} =
            CV.newProp (fn f => NONE)
    in
    fun setSN(f, i : int) = setFn(f, SOME i)
    val getSN = getSN
    end

    (*
     * We only transform functions (i.e. they are safe) if:
     * 1) They are known
     * 2) At each call site, that site is not shared with any other function
     * 3) They are never passed as an argument to an unsafe function
     * 4) They are not shared in any parameter to a function
     *)
    local
        val {setFn, getFn=getSafe, ...} =
            CV.newProp (fn f => not(CFACPS.isEscaping f))
    in
    (*
     * In addition to setting the safety property to false, this also return true if
     * the previous safety value was true.
     *)
    fun setUnsafe(f) = (
        if (getSafe f)
        then (
            if !closureConversionDebug andalso not(CFACPS.isEscaping f)
            then print (concat[CV.toString f, " set unsafe.\n"])
            else ();
            setFn(f, false);
            true)
        else false)
    val getSafe=getSafe
    end

    fun setSafety (CPS.MODULE{body, ...}) = let
          fun handleFun (f) =
              case CFACPS.valueOf f
               of a as CFACPS.LAMBDAS(s) => if (VSet.numItems s = 1)
                               then (false)
                               else let
                                       val items = VSet.listItems s
                                   in
                                       List.foldl (fn (f, b) => b orelse setUnsafe f) (setUnsafe f) items
                                   end
                | _ => (setUnsafe f)
          fun doLambda (CPS.FB{f, params, rets, body}) =
              if getSafe f
              then let
                      val paramsChanged = List.foldl (fn (f, b) => b orelse handleFun f) false params
                      val retsChanged = List.foldl (fn (f, b) => b orelse handleFun f) false rets
                  in
                      paramsChanged orelse retsChanged orelse doExp body
                  end
              else let
                      val paramsChanged = List.foldl (fn (f, b) => b orelse setUnsafe f) false params
                      val retsChanged = List.foldl (fn (f, b) => b orelse setUnsafe f) false rets
                  in
                      paramsChanged orelse retsChanged orelse doExp body
                  end
          and doExp (CPS.Exp(_, e)) = (case e 
                 of CPS.Let(xs, _, e) => (doExp e)
                  | CPS.Fun(fbs, e) => (List.foldl (fn (fb,b) => b orelse doLambda fb) false fbs orelse doExp e)
                  | CPS.Cont(fb, e) => (doLambda fb orelse doExp e)
                  | CPS.If(_, e1, e2) => (doExp e1 orelse doExp e2)
                  | CPS.Switch(_, cases, dflt) => let
                        val casesChanged = List.foldl (fn (c, b) => b orelse doExp (#2 c)) false cases
                    in
                        case dflt
                         of SOME e => casesChanged orelse doExp e
                          | _ => casesChanged
                    end
                  | CPS.Apply (f, _, _) => handleFun f
                  | CPS.Throw (k, _) => handleFun k
                (* end case *))
          fun loop () = let
              val changed = doLambda body
          in
              if changed
              then loop()
              else ()
          end
    in
        loop ()
    end

    (* The FV property maps from a function to a Varmap from the (raw) free variables
     * to their first and last used times (SNs). The initial values are set to
     * the current function's SN (else they would not be free!), but the last used
     * time will be updated during the analysis of the static nesting of the
     * functions.
     *)
    val {setFn=setFVMap, getFn=getFVMap, ...} =
        CV.newProp (fn f => let
                           val fvSet = FreeVars.envOfFun f
                           val fut = valOf (getSN f)
                       in
                           VSet.foldl (fn (x, m) => VMap.insert (m, x, (fut, fut)))
                           VMap.empty fvSet                           
                       end)

    (* The slot count is the number of available registers to use for extra closure
     * parameters.
     * Initialize:
     * - 1 for any lambda with unknown call sites
     * - otherwise, (Target.availRegs - #params)
     *
     * Loop to discover the final count per section 4.3
     *)
    val {setFn=setSlot, getFn=getSlot, ...} =
        CV.newProp (fn f =>
                       if not(getSafe f)
                       then 1
                       else Target.availRegs - (
                            case CV.kindOf f
                             of C.VK_Fun (C.FB{params, rets,...}) =>
                                (List.length params + List.length rets)
                              | C.VK_Cont (C.FB{params, rets,...}) =>
                                (List.length params + List.length rets)
                              | k => raise Fail (concat["Variable: ",
                                                        CV.toString f,
                                                        " cannot have slots because it is of kind: ",
                                                        C.varKindToString k])))

    fun setSNs (CPS.MODULE{body, ...}) = let
          fun doLambda (CPS.FB{f, params, rets, body}, i) = (
              setSN (f, i);
              doExp (body, i))
          and doCont (CPS.FB{f, params, rets, body}, i) = f::doExp (body, i)
          and doExp (CPS.Exp(_, e), i) = (case e 
                 of CPS.Let(xs, _, e) => (doExp (e, i))
                  | CPS.Fun(fbs, e) => (List.foldl (fn (f,l) => l@doLambda (f,i+1)) [] fbs) @ doExp (e, i)
                  | CPS.Cont(fb, e) => (doCont (fb, i+1) @ doExp (e, i))
                  | CPS.If(_, e1, e2) => (doExp (e1, i) @ doExp (e2, i))
                  | CPS.Switch(_, cases, dflt) => (
		      (List.foldl (fn (c,l) => l@doExp(#2 c, i)) [] cases) @
                      (case dflt of NONE => []
                                  | SOME e => doExp (e, i)))
                  | CPS.Apply _ => []
                  | CPS.Throw _ => []
                (* end case *))
          val conts = doLambda (body, 1)
          fun handleConts ([], [], i) = ()
            | handleConts ([], later, i) = let
              in
                  (*
                   * The statement about CPS call graphs in 4.1/4.2 is a bit tricky for us.
                   * Our continutations can take multiple continuations as arguments, which
                   * means that while we cannot have statically mutually recursive continuations,
                   * dynamically they can be (and often are!).
                   *
                   * This case implements a somewhat-arbitrary tie-breaker. If there is a set
                   * of continuations we are not making progress on, just set the stage number
                   * of the first one in the list based on the knowledge we do have so far and then
                   * continue with the rest.
                   *)
                  if (List.length later = i)
                  then let
                          val first::rest = later
                          val CFACPS.Known(s) = CFACPS.callersOf first
                      in
                          setSN (first, 1 + VSet.foldl (fn (f, i) => Int.max (i,
                                                                              case getSN f
                                                                               of NONE => 0
                                                                                | SOME i' => i'))
                                        0 s);
                          handleConts (List.rev rest, [], i-1)
                      end
                  else handleConts (List.rev later, [], List.length later)
              end
            | handleConts (k::conts, later, i) = let
                  val callers = CFACPS.callersOf k
              in
                  case callers
                   of CFACPS.Unknown => (setSN(k, 1);
                                         handleConts (conts, later, i))
                    | CFACPS.Known s => let
                          val f = VSet.find (fn f => not (isSome (getSN f))) s
                      in
                      if not (isSome (f))
                      then (setSN(k, 1 + VSet.foldl (fn (f,i) => Int.max (i, valOf (getSN f)))
                                                    0 s);
                            handleConts (conts, later, i))
                      else (handleConts (conts, k::later, i))
                      end
              end
    in
        handleConts (conts, [], 0)
    end

    (* Also add fut/lut properties to variables, per function.
     * Need to add an FV->{fut,lut} map to each function variable.
     * a. Do it as a depth-first traversal. At the leaf, all FVs basically have the SN(f) for their fut/lut
     * b. Return the union of the var-maps from the other functions. Collisions get the min(fut) but the max(lut)
     * c. In the intermediate nodes, if the FV is used, fut = SN(f), else fut=lut(union-map)
     *)
    fun updateLUTs (CPS.MODULE{body, ...}) = let
        fun mergeFVMaps (maps) = let
            fun mergeMaps ([], final) = final
              | mergeMaps (m::maps, final) =
                mergeMaps (maps,
                           (VMap.foldli (fn (v, p as (fut, lut), final) =>
                                            case VMap.find (final, v)
                                             of NONE => VMap.insert (final, v, p)
                                              | SOME (fut', lut') =>
                                                VMap.insert (final, v, (Int.min (fut, fut'),
                                                                        Int.max (lut, lut'))))
                                        final m))
        in
            case maps
             of [] => VMap.empty
              | [m] => m
              | m::ms => mergeMaps (ms, m)
        end
        fun doLambda (CPS.FB{f, params, rets, body}) = let
            val childMap = doExp body
            val (newMap, retMap) =
                VMap.foldli (fn (v, p as (fut, lut), (newMap, retMap)) => 
                                case VMap.find (retMap, v)
                                 of NONE => (newMap,
                                             VMap.insert (retMap, v, p))
                                  | SOME (_, lut') => 
                                    if (lut' > lut)
                                    then (VMap.insert (newMap, v, (fut, lut')),
                                          retMap)
                                    else (newMap, retMap))
                            (VMap.empty, childMap) (getFVMap f)
        in
            setFVMap (f, newMap);
            retMap
        end
        and doExp (CPS.Exp(_, e)) = (
            case e 
             of CPS.Let(xs, _, e) => (doExp e)
              | CPS.Fun(fbs, e) => (mergeFVMaps ((doExp e)::(List.map doLambda fbs)))
              | CPS.Cont(fb, e) => (mergeFVMaps([doLambda fb, doExp e]))
              | CPS.If(_, e1, e2) => (mergeFVMaps([doExp e1, doExp e2]))
              | CPS.Switch(_, cases, dflt) => let
                    val caseMaps = List.map (fn c => doExp (#2 c)) cases
                    val l = case dflt of NONE => caseMaps
                                       | SOME e => (doExp e)::caseMaps
                in
                    mergeFVMaps l
                end
              | CPS.Apply _ => VMap.empty
              | CPS.Throw _ => VMap.empty
        (* end case *))
    in
        doLambda body
    end

    fun getSafeFuns (CPS.MODULE{body, ...}) = let
        fun doLambda (CPS.FB{f, params, rets, body}) = f::(doExp body)
        and doExp (CPS.Exp(_, e)) = (
            case e 
             of CPS.Let(xs, _, e) => (doExp e)
              | CPS.Fun(fbs, e) => (doExp e @ (List.foldl (fn (fb,l) => l @ doLambda fb) [] fbs))
              | CPS.Cont(fb, e) => (doLambda fb @ doExp e)
              | CPS.If(_, e1, e2) => (doExp e1 @ doExp e2)
              | CPS.Switch(_, cases, dflt) => let
                    val cases = List.foldl (fn (c,l) => l@doExp (#2 c)) [] cases
                in
                    case dflt of NONE => cases
                               | SOME e => (doExp e)@cases
                end
              | CPS.Apply _ => []
              | CPS.Throw _ => []
        (* end case *))
        val funs = doLambda body
        val funs = List.filter (fn (f) => getSafe f) funs
    in
        funs
    end
                                
    (* Assign the # of slots to functions for their free variables
     * a. Initialize S(f) = max(AVAIL_REG - arg_count(f), 0)  for known, = 1 for escaping
     * b. Iterate S(f) = min ({T(g, f) | g \in V(f)} U {S(f)})
     * c. where T(g, f) = max(1, S(g)- |FV(g)-  FV(f)|)
     * V(f) is all of the callersOf f where f is also free in that function
     * Q: How do I handle that two functions that share a call site also share the number
     * of slots?
     *)
    fun setSlots (funs) = let
        fun updateSlotCount(f) = let
            val fFVs = (FreeVars.envOfFun f)
            val fFVfuns = VSet.filter (fn v => case CV.kindOf v
                                                of C.VK_Cont _ => true (* ? *)
                                                 | C.VK_Fun _ => true
                                                 | _ => false) fFVs
            val CFACPS.Known(fPreds) = CFACPS.callersOf f
            val fFreePreds = VSet.intersection (fPreds, fFVfuns)
            fun T(g) = Int.max (1, (getSlot g) - VSet.numItems (VSet.difference ((FreeVars.envOfFun g),
                                                                          fFVs)))
            and S(f) = VSet.foldl (fn (g, i) => Int.min (i, T(g))) (getSlot f) fFreePreds
            val newCount = S(f)
        in
            if (newCount <> getSlot f)
            then (setSlot (f, newCount); true)
            else false
        end
        fun loop () = let
            val changed = List.foldl (fn (f, b) => b orelse updateSlotCount f) false funs
        in
            if changed
            then loop()
            else ()
        end
    in
        loop()
    end

    (*
     * The params properties list the new parameters for a given SAFE function.
     * It is a VMap from old FV-name to new param-name.
     *)
    local
        val {setFn=setParams, getFn=getParams, ...} =
            CV.newProp (fn f => raise Fail (concat[CV.toString f, " has no params and ", if getSafe f then "is safe" else "is not safe"]))
    in
    val getParams: CV.var -> (CV.var VMap.map) = getParams
    val setParams=setParams
    end

    (*
     * So, if the number of FVs is < the number of slots available
     * then, we just add all of those variables as new, additional parameters to the function
     * else, add (slots-1) of those FVs, preferring lowest LUT then lowest FUT
     *)
    fun computeParams funs = let
        fun findBest (n, map) = let
            fun firstBigger ((_, (fut1, lut1)), (_, (fut2, lut2))) =
                (lut1 > lut2) orelse ((lut1 = lut2) andalso (fut1 > fut2))
            val sorted = ListMergeSort.sort firstBigger (VMap.listItemsi map)
        in
            List.map (fn (p, _) => p) (List.take (sorted, n))
        end
        fun computeParam f = let
            val map = getFVMap f
        in
            case VMap.numItems map
             of 0 => (setParams (f, VMap.empty))
              | n => (if n <= getSlot f
                      then (ST.tick cntFunsClosed;
                            setParams (f, 
                                       VMap.foldli (fn (p, _, m) => VMap.insert (m, p, CV.copy p))
                                       VMap.empty map))
                      else (let
                                val _ = ST.tick cntFunsPartial
                                val toCopy = findBest (n-1, map)
                            in
                                setParams (f, 
                                           List.foldl (fn (p, m) => VMap.insert (m, p, CV.copy p))
                                                       VMap.empty toCopy)
                            end))
        end
    in
        List.app computeParam funs
    end


    (*
     * Simply adds the new parameters to the function.
     * Note that they retain their old names because this allows the type-based
     * iterative fixup to correctly globally update all associated types without
     * performing CFA a second time to give us enough info to fix them up directly.
     *)
    fun addParams (C.MODULE{name,externs,body}) = let
        fun convertFB (C.FB{f, params, rets, body}) =
            if getSafe f
            then let
                    val newArgsMap = getParams f
                    val oldNewList = VMap.listItemsi newArgsMap
                    val newParams = List.map (fn (p, _) => p) oldNewList
                    val _  = List.app (fn p => (List.app (fn p' => if CV.same(p, p')
                                                                   then raise Fail (concat["In ", CV.toString f, " double-adding ",
                                                                                           CV.toString p])
                                                                   else ()) newParams)) params
                    val params = params @ newParams
                    val body = convertExp body
                    val origType = CV.typeOf f
                    val newType = case origType
                                   of CTy.T_Fun (_, retTys) =>
                                      CTy.T_Fun(List.map CV.typeOf params, retTys)
                                    | CTy.T_Cont (_) => 
                                      CTy.T_Cont(List.map CV.typeOf params)
                                    | x => raise Fail (concat["Non-function type - ", CV.toString f,
                                                              ":", CPSTyUtil.toString (CV.typeOf f)])
                    val _ = CV.setType (f, newType)
                    val _ = if !closureConversionDebug
                            then print (concat[CV.toString f, " added params\nOrig: ",
                                               CPSTyUtil.toString origType, "\nNew : ",
                                               CPSTyUtil.toString newType, "\nParams   :",
                                               String.concatWith "," (List.map CV.toString params), "\nNewParams :",
                                               String.concatWith "," (List.map CV.toString newParams), "\nNParams:",
                                               String.concatWith "," (List.map CV.toString (List.map (fn (_, p) => p) oldNewList)),
                                               "\n"])
                            else ()
                in
                    C.FB{f=f, params=params, rets=rets, body=body}
                end
            else C.FB{f=f, params=params, rets=rets, body=convertExp body}
        and convertExp (C.Exp(ppt,t)) = (
            case t
             of C.Let (lhs, rhs, exp) => C.mkLet(lhs, rhs,
                                                 convertExp exp)
              | C.Fun (lambdas, exp) => (C.mkFun (List.map (fn fb => convertFB (fb)) lambdas, convertExp (exp))) 
              | C.Cont (lambda, exp) => (C.mkCont (convertFB (lambda), convertExp (exp)))
              | C.If (cond, e1, e2) => C.mkIf(cond,
		                              convertExp(e1),
		                              convertExp(e2))
              | C.Switch(x, cases, dflt) => 
	        C.mkSwitch (x,
		            List.map (fn (l, e) => (l, convertExp(e))) cases,
		            Option.map (fn e => convertExp(e)) dflt)
              | e => C.mkExp e)
    in
        C.MODULE{
        name=name, externs=externs,
        body = convertFB body}
    end

    fun getSafeCallTarget f =
        case CFACPS.valueOf f
         of a as CFACPS.LAMBDAS(s) =>
            if (VSet.numItems s = 1)
            then let
                    val target = hd (VSet.listItems s)
                in
                    if getSafe target
                    then SOME(target)
                    else NONE
                end
            else NONE
          | _ => NONE

    (* 
     * The function types on all variables that are equivalent to the
     * safe/converted functions need to be fixed up to have the same
     * type as the function now has.
     *
     * Because this change can affect parameters to functions, we need
     * to push these changes through iteratively until the types no longer
     * change.
     * NOTE: this code is a simpler version of the code appearing in
     * signature fixup in cps-opt/arity-raising.
     *)
    fun propagateFunChanges (C.MODULE{body=fb,...}) = let
        val changed = ref false
        fun transformParam(param) = let
            fun getParamFunType (l, orig) =let
                val lambdas = CPS.Var.Set.listItems l
             in
                case List.length lambdas
                 of 0 => orig
                  | 1 => (case getSafeCallTarget (hd lambdas)
                           of SOME a => CV.typeOf a
                            | NONE => orig)
                  | _ => orig
            end
            fun buildType (CPSTy.T_Tuple (heap, tys), cpsValues) = let
                fun updateSlot (origTy, cpsValue) = (
                    case cpsValue
                     of CFACPS.LAMBDAS(l) => getParamFunType (l, origTy)
                      | CFACPS.TUPLE (values) => buildType (origTy, values)
                      | _ => origTy
                (* end case *))
                val newTys = ListPair.map updateSlot (tys, cpsValues)
            in
                CPSTy.T_Tuple (heap, newTys)
            end
              | buildType (ty, _) = ty
        in
            case CFACPS.valueOf param
             of a as CFACPS.LAMBDAS(l) => let
                    val newType = getParamFunType (l, CV.typeOf param)
                in
                    if CPSTyUtil.equal (CV.typeOf param, newType)
                    then ()
                    else (changed := true;
                          print (concat["Changed LAMBDAS(", CFACPS.valueToString a, ") for ",
                                        CV.toString param, " from: ",
                                        CPSTyUtil.toString (CV.typeOf param), " to: ",
                                        CPSTyUtil.toString newType, "\n"]);
                          CV.setType (param, newType))
                end

              | CFACPS.TUPLE(values) => let
                    val newType = buildType (CV.typeOf param, values)
                in
                    if CPSTyUtil.equal (CV.typeOf param, newType)
                    then ()
                    else (
                          print (concat["Changed TUPLE for ", CV.toString param, " from: ",
                                        CPSTyUtil.toString (CV.typeOf param), " to: ",
                                        CPSTyUtil.toString newType, "\n"]);
                          changed := true;
                          CV.setType (param, newType))
                end
              | _ => ()
        end
        and handleLambda(func as C.FB{f, params, rets, body}) = let
            val origType = CV.typeOf f
            val _ = List.app transformParam params
            val _ = List.app transformParam rets
	    val newType = case origType
                           of CTy.T_Fun _ => CTy.T_Fun (List.map CV.typeOf params, List.map CV.typeOf rets)
                            | CTy.T_Cont _ => CTy.T_Cont (List.map CV.typeOf params)
                            | _ => raise Fail (concat["Non-function type variable in a FB block: ", CV.toString f, " : ",
                                                      CPSTyUtil.toString origType])
            val _ = if CPSTyUtil.equal (origType, newType)
                    then ()
                    else (
                          print (concat["Changed FB type for ", CV.toString f, " from: ",
                                        CPSTyUtil.toString origType, " to: ",
                                        CPSTyUtil.toString newType, "\n"]))
            val _ = CV.setType (f, newType)
	in
            walkBody (body)
	end
        and walkBody (C.Exp(_, e)) = (
            case e
             of C.Let (lhs, _, e) => (walkBody (e))
              | C.Fun (lambdas, body) => (List.app handleLambda lambdas; walkBody (body))
              | C.Cont (f, body) => (handleLambda f; walkBody (body))
              | C.If (_, e1, e2) => (walkBody (e1); walkBody (e2))
              | C.Switch (_, cases, body) => (
                List.app (fn (_, e) => walkBody (e)) cases;
                Option.app (fn x => walkBody (x)) body)
              | C.Apply (_, _, _) => ()
              | C.Throw (_, _) => ())
        (*
         * If we change the signature of a function that was passed in as an argument
         * to an earlier function, we may need to go back and fix it up. Therefore, we
         * iterate until we reach a fixpoint.
         *)
        fun loopParams (fb) = (
            handleLambda (fb);
            if (!changed)
            then (changed := false; print "LOOP!\n"; loopParams (fb))
            else ())
    in
        loopParams(fb)
    end

    (* env is a VMap.empty, CV.Var->CV.Var *)
    fun rename (env, x, y) = (
	(* every use of x will be replaced by a use of y *)
	  VMap.insert(env, x, y))

    (* apply a substitution to a variable *)
    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => let
                  val xty = CV.typeOf x
                  val yty = CV.typeOf y
              in
                  if not(CPSTyUtil.equal (xty, yty))
                  then CV.setType (y, xty)
                  else ();
                  y
              end
	    | NONE => x
	  (* end case *))

    (* apply a substitution to a list of variables *)
    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst(env, x) :: subst'(env, xs)

    (*
     * Convert does two things:
     * - Changes the FB param names from the "original" ones with associated
     * CFA info (so we could type-correct them!) to new ones, with the
     * copied type blasted to be equal to the one from the source
     * - Changes Apply/Throw to safe functions to take the new parameters
     *)
    fun convert (env, C.MODULE{name,externs,body}) = let
        fun convertFB (env, C.FB{f, params, rets, body}) =
            if getSafe f
            then let
                    val newArgsMap = getParams f
                    val oldNewList = VMap.listItemsi newArgsMap
                    val params = subst' (newArgsMap, params)
                    val env = List.foldr (fn ((x, y), m) => rename (m, x, y)) env oldNewList
                    val body = convertExp (env, body)
                in
                    C.FB{f=f, params=params, rets=rets, body=body}
                end
            else C.FB{f=f, params=params, rets=rets, body=convertExp (env, body)}
        and convertExp (env, C.Exp(ppt,t)) = (
            case t
             of C.Let (lhs, rhs, exp) => C.mkLet(lhs, convertRHS(env, rhs),
                                                 convertExp (env, exp))
              | C.Fun (lambdas, exp) => (C.mkFun (List.map (fn fb => convertFB (env, fb)) lambdas, convertExp (env, exp))) 
              | C.Cont (lambda, exp) => (C.mkCont (convertFB (env, lambda), convertExp (env, exp)))
              | C.If (cond, e1, e2) => C.mkIf(CondUtil.map (fn x => subst(env, x)) cond,
		                              convertExp(env, e1),
		                              convertExp(env, e2))
              | C.Switch(x, cases, dflt) => let
	            val x = subst(env, x)
	        in
	            C.mkSwitch (x,
		                List.map (fn (l, e) => (l, convertExp(env, e))) cases,
		                Option.map (fn e => convertExp(env, e)) dflt)
	        end
              | C.Apply(f, args, conts) => let
	            val f' = subst(env, f)
	            val args = subst'(env, args)
	            val conts = subst'(env, conts)
	        in
                    case getSafeCallTarget f
                     of SOME a => let
                            val newArgsMap = getParams a
                            val newArgs = subst' (env, VMap.foldri (fn (v, _, l) => v::l) [] newArgsMap)
                            val args = args @ newArgs
                            val _ = if !closureConversionDebug
                                    then print (concat["Apply to safe call target through: ", CV.toString f,
                                                       " renamed to: ", CV.toString f', " safe call target named: ",
                                                       CV.toString a, "\n"])
                                    else ()       
                        in
                            C.mkApply (f', args, conts)
                        end
                      | NONE => (
                        if !closureConversionDebug
                        then print (concat["Apply to unsafe call target through: ", CV.toString f,
                                           " renamed to: ", CV.toString f', "\n"])
                        else ();
                        C.mkApply(f', args, conts))
	        end
              | C.Throw(k, args) => let
	            val k' = subst(env, k)
	            val args = subst'(env, args)
	        in
                    case getSafeCallTarget k
                     of SOME a => let
                            val newArgsMap = getParams a
                            val newArgs = subst' (env, VMap.foldri (fn (v, _, l) => v::l) [] newArgsMap)
                            val args = args @ newArgs
                            val _ = if !closureConversionDebug
                                    then print (concat["Throw to safe call target through: ", CV.toString k,
                                                       " renamed to: ", CV.toString k', " safe call target named: ",
                                                       CV.toString a, "\n"])
                                    else ()       
                        in
                            C.mkThrow (k', args)
                        end
                      | NONE => (
                        if !closureConversionDebug
                        then print (concat["Throw to unsafe call target through: ", CV.toString k,
                                           " renamed to: ", CV.toString k', "\n"])
                        else ();
                        C.mkThrow (k', args))
	        end)
        and convertRHS(env, C.Var(vars)) = C.Var(subst'(env,vars))
          | convertRHS(env, C.Cast(ty,v)) = C.Cast(ty,subst(env,v))
          | convertRHS(env, C.Select(i,v)) = C.Select(i,subst(env,v))
          | convertRHS(env, C.Update(i,v1,v2)) = C.Update(i,subst(env,v1),subst(env,v2))
          | convertRHS(env, C.AddrOf(i,v)) = C.AddrOf(i, subst(env,v))
          | convertRHS(env, C.Alloc(ty,vars)) = C.Alloc(ty, subst'(env,vars))
          | convertRHS(env, C.Promote (v)) = C.Promote(subst(env,v))
          | convertRHS(env, C.Prim(p)) = C.Prim(PrimUtil.map (fn x => subst(env, x)) p)
          | convertRHS(env, C.CCall (var, vars)) = C.CCall (var, subst'(env,vars))
          | convertRHS(env, C.VPLoad(off,var)) = C.VPLoad (off, subst(env,var))
          | convertRHS(env, C.VPStore (off, v1, v2)) = C.VPStore (off, subst(env, v1), subst(env,v2))
          | convertRHS(env, C.VPAddr (off, var)) = C.VPAddr (off, subst(env, var))
          | convertRHS(env, x) = x
    in
        C.MODULE{
        name=name, externs=externs,
        body = convertFB (VMap.empty, body)}
    end

    fun transform module =
        if !enableClosureConversion
        then let
                val _ = FreeVars.analyze module
                val _ = CFACPS.analyze module
                val _ = setSafety module
                val _ = setSNs module
                val _ = updateLUTs module
                val funs = getSafeFuns module
                val _ = List.app (fn f => print (concat[CV.toString f, " is safe\n"])) funs
                val _ = setSlots funs
                val _ = computeParams funs
                val module = addParams module
                             val _ = print "done add\n"
                val _ = propagateFunChanges module
                             val _ = print "done prop\n"
(*                val _ = fixupTypes module *)
                val module = convert (VMap.empty, module)
                val _ = print (concat ["Closed: ", Int.toString (ST.count cntFunsClosed)])
                val _ = print (concat ["Partial-closed: ", Int.toString (ST.count cntFunsPartial)])
                val _ = CFACPS.clearInfo module
                val _ = FreeVars.clear module
	        val _ = CPSCensus.census module
            in
                module
            end
        else module
end
