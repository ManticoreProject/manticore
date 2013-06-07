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
 * registers, including callee-save registers for continuations.
 * Then, we rely on simple flat closure conversion to handle both unknown
 * functions (as in the SSCC work) and for known functions that have too
 * many FVs (different from the SSCC work, which uses linked closures).
 *
 * STATUS:
 * This work was started by Carsen Berger, but has not been completed.
 * At present, it handles basic cases, but fails to pass the full set of
 * regression tests. It should be considered a reasonable starting point,
 * but as we do not have a characterization of the failure cases, starting
 * from scratch is also a reasonble strategy.
 *)

functor ClosureConvertFn (Target : TARGET_SPEC) : sig

    val transform : CPS.module -> CPS.module
    val newConvert : CPS.module -> CFG.module

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

    structure FV = FreeVars

  (***** Statistics *****)
    val cntFunsClosed		= ST.newCounter "clos:num-funs-closed"
    val cntFunsPartial		= ST.newCounter "clos:num-funs-partially-closed"

  (***** controls ******)
    val enableClosureConversion = ref false
    val closureConversionDebug = ref true

    val nCalleeSaveRegs = 3

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
     * 1+ SN(g) if f is a function and g encloses f
     * 1+ max{SN(g) | g \in CFA.callersOf k} if k is a continuation
     *)
    local
        val {setFn, getFn=getSN, ...} =
            CV.newProp (fn f => NONE)
    in
    fun setSN(f, i : int) = setFn(f, SOME i)
    val getSN = getSN
    end

    fun getSafe f = (CV.useCount f = CV.appCntOf f andalso
                         case CFACPS.callersOf f of CFACPS.Known _ => true | CFACPS.Unknown => false)
                      
    fun isCont f =
        case CV.typeOf f
         of CTy.T_Cont _ => true
          | _ => false

    (*
     * Any continuation that escapes through any location other than the
     * retK is considered CALLEE_SAVE. This distinction will roughly correspond to
     * partitioning the continuations into user and CPS-introduced continuations.
     *
     * KNOWN corresponds to the SML/NJ's known: appCnt=useCount and all call sites are
     * known. It will be safely modified.
     *
     * All other continuations are ESCAPING, either because they have some CFA-unknown callers
     *)
    datatype contKind = ESCAPING | KNOWN | CALLEE_SAVE
    local
        val {setFn, getFn=getContKind, ...} =
            CV.newProp (fn f => (case CV.kindOf f of C.VK_Cont _ => ()
                                                   | _ => raise Fail (concat[CV.toString f,
                                                                             " is not a VK_Cont, so its contKind must be set.\n"]);
                                 case (CV.useCount f = CV.appCntOf f,
                                       case CFACPS.callersOf f of CFACPS.Known _ => true | CFACPS.Unknown => false)
                                  of (true, true) => KNOWN
                                   | (_, false) => ESCAPING (* called from somewhere that we can't change a signature *)
                                   | (false, true) => CALLEE_SAVE))
    in
    val getContKind = getContKind
    fun setContKind (v,k) = (print (concat[CV.toString v, " set to contKind: ",
                                           case k
                                            of ESCAPING => "escaping\n"
                                             | KNOWN => "known\n"
                                             | CALLEE_SAVE => "callee-save\n"]); setFn (v,k))
    end


    (* The FV property maps from a function to a Varmap from the (raw) free variables
     * to their first and last used times (SNs). The initial values are set to
     * the current function's SN (else they would not be free!), but the last used
     * time will be updated during the analysis of the static nesting of the
     * functions.
     *)
    val {setFn=setFVMap', getFn=getFVMap, ...} =
        CV.newProp (fn f => let
                           val fvSet = FreeVars.envOfFun f
                           val fut = valOf (getSN f)
                       in
                           VSet.foldl (fn (x, m) => VMap.insert (m, x, (fut, fut)))
                           VMap.empty fvSet                           
                       end)
    (* +DEBUG *)

	fun freeVarsToString fvmap = let
	    fun varToString (x,p,s) = concat [s,CPS.Var.toString x, ", "]
	in
	    VMap.foldli varToString "" fvmap
	end  


    fun setFVMap (f, map)= (
	print(concat["Calling setFVMap on ", CPS.Var.toString f, " with ", freeVarsToString map, "\n"]);
	setFVMap' (f,map))

    (* -DEBUG *)
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
                                (List.length params + List.length rets + (List.foldl (fn (r,i) =>
                                                                                         case (getContKind r)
                                                                                          of CALLEE_SAVE => i + nCalleeSaveRegs
                                                                                           | _ => i) 0 rets))
                              | C.VK_Cont (C.FB{params, rets,...}) =>
                                (List.length params + List.length rets + nCalleeSaveRegs)
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

(* +DEBUG *)
    fun prSet s = (
	  print "{";
	  VSet.foldl
	    (fn (x, false) => (print("," ^ CPS.Var.toString x); false)
	      | (x, true) => (print(CPS.Var.toString x); false)
	    ) true s;
	  print "}";
	  print "\n")
(* -DEBUG*)


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
	(* + DEBUG *)
	    val _ = print (concat ["Updating LUT for func: ", CV.toString f,
				   " and its free vars are: "])
	    val _ = prSet (FreeVars.envOfFun f)
	    val _ = print(concat["There are ", Int.toString(VMap.numItems(getFVMap f)), "\n"])
(* - DEBUG *)

            val childMap = doExp body
	    val _ = print(concat["childMap has ", Int.toString(VMap.numItems(childMap)), "\n"])
            val (newMap, retMap) =
                VMap.foldli (fn (v, p as (fut, lut), (newMap, retMap)) => 
                                case VMap.find (retMap, v)
                                 of NONE => (VMap.insert (newMap, v, p),
                                             VMap.insert (retMap, v, p))
                                  | SOME (_, lut') => 
                                    if (lut' > lut)
                                    then (VMap.insert (newMap, v, (fut, lut')),
                                          retMap)
                                    else (VMap.insert (newMap, v, p), retMap))
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
        fun setVarContKind v =
            if isCont v
            then (case CFACPS.valueOf v
                   of a as CFACPS.LAMBDAS(s) => let
                          val target = hd (VSet.listItems s)
                      in
                          print (concat["Because of ", CV.toString target, " "]);
                          setContKind (v, getContKind target)
                      end
                    | cv => (print (concat["CFA value is ", CFACPS.valueToString cv, ", so "]);
                             setContKind (v, ESCAPING)))
            else ()
        fun doLambda (CPS.FB{f, params, rets, body}) = let
            val _ = List.app setVarContKind params
            val _ = List.app setVarContKind rets
        in
            (f::(doExp body))
        end
        and doExp (CPS.Exp(_, e)) = (
            case e 
             of CPS.Let(xs, _, e) => (List.app setVarContKind xs;
                                        doExp e)
              | CPS.Fun(fbs, e) => (doExp e @ (List.foldl (fn (fb,l) => l @ doLambda fb) [] fbs))
              | CPS.Cont(fb, e) => (doLambda fb @ doExp e)
              | CPS.If(_, e1, e2) => (doExp e1 @ doExp e2)
              | CPS.Switch(_, cases, dflt) => let
                    val cases = List.foldl (fn (c,l) => l@doExp (#2 c)) [] cases
                in
                    case dflt of NONE => cases
                               | SOME e => (doExp e)@cases
                end
              | CPS.Apply (_, _, _) => ([])
              | CPS.Throw (_, _) => ([])
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
     * For safe FBs, if the number of FVs is < the number of slots available
     * then, we just add all of those variables as new, additional parameters to the function
     * else, add (slots-1) of those FVs, preferring lowest LUT then lowest FUT
     *
     * For unsafe conts, we add up to the nCalleeSaves of the top-ranked FVs to be added as
     * new parameters for callers to track. Note that they will be passed around as :any,
     * so we must restrict the map to variables of non-raw types.
     *)
    fun computeParams (CPS.MODULE{body, ...}) = let
        fun findBest (n, map) = let
            fun firstBigger ((_, (fut1, lut1)), (_, (fut2, lut2))) =
                (lut1 > lut2) orelse ((lut1 = lut2) andalso (fut1 > fut2))
            val sorted = ListMergeSort.sort firstBigger (VMap.listItemsi map)
        in
            List.map (fn (p, _) => p) (List.take (sorted, n))
        end
        fun computeParam (f, map, i) = let
            val map = getFVMap f
            val i = getSlot f
        in
            case VMap.numItems map
             of 0 => (setParams (f, VMap.empty))
              | n => (if n <= i
                      then (ST.tick cntFunsClosed;
                            setParams (f, 
                                       VMap.foldli (fn (p, _, m) => VMap.insert (m, p, CV.copy p))
                                       VMap.empty map))
                      else (let
                                val _ = ST.tick cntFunsPartial
                                val toCopy = findBest (i-1, map)
                            in
                                setParams (f, 
                                           List.foldl (fn (p, m) => VMap.insert (m, p, CV.copy p))
                                                       VMap.empty toCopy)
                            end))
        end
        fun doLambda (fb as CPS.FB{f, params, rets, body}) = (
            if getSafe f
            then computeParam (f, getFVMap f, getSlot f)
            else (if isCont f
                  then let
                          val map = getFVMap f
                          val map = VMap.filteri (fn (v,_) => case CPSTyUtil.kindOf (CV.typeOf v)
                                                               of CTy.K_RAW => false
                                                                | _ => true) map
                      in
                          computeParam (f, map, nCalleeSaveRegs)
                      end
                  else ());
            (doExp body))
        and doExp (CPS.Exp(_, e)) = (
            case e 
             of CPS.Let(xs, _, e) => (doExp e)
              | CPS.Fun(fbs, e) => (List.app doLambda fbs; doExp e )
              | CPS.Cont(fb, e) => (doLambda fb ; doExp e)
              | CPS.If(_, e1, e2) => (doExp e1 ; doExp e2)
              | CPS.Switch(_, cases, dflt) => (
                List.app (fn c => doExp (#2 c)) cases;
                Option.app doExp dflt)
              | CPS.Apply _ => ()
              | CPS.Throw _ => ()
        (* end case *))
    in
        doLambda body
    end

    (*
     * We can remove a function from the FV list if:
     * 1) it is a function name
     * 2) it is safe
     * 3) all of its params have been passed in
     *)
    fun reduceParams funs = let
        fun isEligible f =
            case (CV.kindOf f, getSafe f)
             of (C.VK_Fun _, true) => true
              | (C.VK_Cont _, true) => true
              | _ => false
        fun reduceParam f = let
            val pset = VMap.foldli (fn (v,_,s) => VSet.add(s,v)) VSet.empty (getParams f)
            val (params,funs) = VSet.foldl (fn (f, (ps,fs)) => if isEligible f
                                                               then (ps, VSet.add (fs,f))
                                                               else (VSet.add (ps,f), fs))
                                           (VSet.empty, VSet.empty) pset
            val keepFuns = VSet.foldl (fn (v,s) => if VSet.isSubset (FreeVars.envOfFun v, pset)
                                                   then s
                                                   else (VSet.add (s,v)))
                           VSet.empty funs
            val pset = VSet.union (params, keepFuns)
            fun mustKeep (f,_) = VSet.member (pset, f)
        in
            setParams (f, VMap.filteri mustKeep (getParams f))
        end
    in
        List.app reduceParam funs
    end

    (*
     * The params properties list the new parameters for a given SAFE function.
     * It is a VMap from old FV-name to new param-name.
     *)
    local
        val {setFn=setCalleeParams, getFn=getCalleeParams, ...} =
            CV.newProp (fn f => raise Fail (concat[CV.toString f, " has no callee-save params associated with it."]))
    in
    val getCalleeParams: CV.var -> (CV.var list) = getCalleeParams
    val setCalleeParams=setCalleeParams
    end

    (*
     * Simply adds the new parameters to the function.
     * Note that they retain their old names because this allows the type-based
     * iterative fixup to correctly globally update all associated types without
     * performing CFA a second time to give us enough info to fix them up directly.
     *
     * Also, notice the callee-save register additions. They are made in two places:
     * - All _Fun types get (length rets)*nCalleeSaveRegs additional params of type :any
     * - All _Cont types that are NOT safe get three additional params, again of
     * type :any
     * These conts are restricted to just the ones that are non-escaping.
     *)
    fun addParams (C.MODULE{name,externs,body}) = let
        fun convertFB (C.FB{f, params, rets, body}) = let
            fun genCalleeSaveParams ret = let
                fun genCalleeSaveParam 0 = []
                  | genCalleeSaveParam i = ((CV.new (CV.nameOf ret ^ Int.toString i,
                                                     CTy.T_Any))::(genCalleeSaveParam (i-1)))
                val nParams = (case getContKind ret
                                of CALLEE_SAVE => (print (concat[CV.toString ret, " is CS\n"]); nCalleeSaveRegs)
                                 | ESCAPING => (print (concat[CV.toString ret, " is ESCAPING!\n"]); 0)
                                 | KNOWN => (print (concat[CV.toString ret, " is KNOWN!\n"]); 0))
                val _ = print (concat[CV.toString ret, " callee-saves: ", Int.toString nParams, "\n"])
                val params = genCalleeSaveParam nParams
                val _ = case CV.typeOf ret
                         of CTy.T_Cont orig => CV.setType (ret, CTy.T_Cont (orig @ List.map (fn v => CV.typeOf v) params))
                          | x => raise Fail (concat["Non-cont type - ", CV.toString ret,
                                                    ":", CPSTyUtil.toString (CV.typeOf ret)])

                val _ = setCalleeParams (ret, params)
            in
                params
            end
            fun genContCalleeSaveParams () = let
                val newArgsMap = getParams f
                val oldNewList = VMap.listItemsi newArgsMap
                val newParams = List.map (fn (p, _) => p) oldNewList
                val nParamsMissing = nCalleeSaveRegs - List.length newParams
            in
                case Int.compare (nParamsMissing, 0)
                 of EQUAL => newParams
                  | GREATER => newParams @ List.tabulate (nParamsMissing, (fn _ => CV.new ("ignoredCalleeSave", CTy.T_Any)))
                  | LESS => raise Fail (concat["Continuation ", CV.toString f, " is callee-save but has too many new params: ",
                                               String.concat(List.map CV.toString newParams)])
            end
            val calleeSaveParams = (
                case CV.typeOf f
                 of CTy.T_Fun _ => List.concat (List.map genCalleeSaveParams rets)
                  | CTy.T_Cont _ => (case getContKind f of CALLEE_SAVE => genContCalleeSaveParams() | _ => [])
                  | x => raise Fail (concat["Non-function type - ", CV.toString f,
                                            ":", CPSTyUtil.toString (CV.typeOf f)]))
        in
            if getSafe f
            then let
                    val newArgsMap = getParams f
                    val oldNewList = VMap.listItemsi newArgsMap
                    val newParams = List.map (fn (p, _) => p) oldNewList
                    val params' = params @ newParams @ calleeSaveParams
                    val body = convertExp body
                    val origType = CV.typeOf f
                    val newType = case origType
                                   of CTy.T_Fun (_, retTys) =>
                                      CTy.T_Fun(List.map CV.typeOf params', retTys)
                                    | CTy.T_Cont (_) => 
                                      CTy.T_Cont(List.map CV.typeOf params')
                                    | x => raise Fail (concat["Non-function type - ", CV.toString f,
                                                              ":", CPSTyUtil.toString (CV.typeOf f)])
                    val _ = CV.setType (f, newType)
                    val _ = if !closureConversionDebug
                            then print (concat[CV.toString f, " safe fun added params\nOrig: ",
                                               CPSTyUtil.toString origType, "\nNew : ",
                                               CPSTyUtil.toString newType, "\nParams   :",
                                               String.concatWith "," (List.map CV.toString params), "\nNewParams :",
                                               String.concatWith "," (List.map CV.toString newParams), "\nCalleeSaveP:",
                                               String.concatWith "," (List.map CV.toString calleeSaveParams), "\nMap:",
                                               String.concatWith "," (List.map CV.toString (List.map (fn (_, p) => p) oldNewList)),
                                               "\n"])
                            else ()
                    val _  = List.app (fn p => (List.app (fn p' => if CV.same(p, p')
                                                                   then raise Fail (concat["In ", CV.toString f, " double-adding ",
                                                                                           CV.toString p])
                                                                   else ()) newParams)) params
                in
                    C.FB{f=f, params=params', rets=rets, body=body}
                end
            else let
                    val params = params @ calleeSaveParams
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
                            then print (concat[CV.toString f, " non-safe fun added params\nOrig: ",
                                               CPSTyUtil.toString origType, "\nNew : ",
                                               CPSTyUtil.toString newType, "\nParams   :",
                                               String.concatWith "," (List.map CV.toString params), "\n"])
                            else ()
                in
                    C.FB{f=f, params=params, rets=rets, body=body}
                end
        end
        and convertExp (C.Exp(ppt,t)) = (
            case t
             of C.Let (lhs, rhs, exp) => C.mkLet(lhs, rhs,
                                                 convertExp exp)
              | C.Fun (lambdas, exp) => (C.mkFun (List.map (fn fb => convertFB (fb)) lambdas, convertExp (exp)))
              | C.Cont (lambda, exp) => (C.mkCont (convertFB lambda, convertExp (exp)))
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

    fun getSafeCallTarget' f =
        case CFACPS.valueOf f
         of a as CFACPS.LAMBDAS(s) => let
                    val target = hd (VSet.listItems s)
                in
                    if isCont f orelse VSet.numItems s = 1
                    then SOME(target)
                    else NONE
                end
          | _ => NONE

    fun getSafeCallTarget f =
        case CFACPS.valueOf f
         of a as CFACPS.LAMBDAS(s) => let
                    val target = hd (VSet.listItems s)
                in
                    if VSet.numItems s > 0 andalso getSafe target
                    then (print (concat["GSCT: ", CV.toString f, " => ", CV.toString target, "\n"]); SOME(target))
                    else NONE
                end
          | _ => NONE

    fun fixLet (lhs, rhs) = let
        (* Only return a changed type if it's likely to be different.
         * For many RHS values, it's neither straightforward nor necessary to
         * compute a new type value, as it can't have been changed by the
         * flattening transformation.
         *)
        fun typeOfRHS(C.Var ([v])) = SOME (CV.typeOf v)
          | typeOfRHS(C.Var (vars)) = NONE
          | typeOfRHS(C.Cast (typ, var)) = (* if the cast is or contains a function type,
                                            * we may have updated it in an incompatible way
                                            * for the cast, so just fill in our type.
                                            * This frequently happens around the generated _cast
                                            * for ropes code
                                            *)
            let
                val rhsType = CV.typeOf var
                fun cleanup (CTy.T_Tuple (b, dstTys), CTy.T_Tuple (b', srcTys)) = let
                    val dstLen = List.length dstTys
                    val srcLen = List.length srcTys
                    val (dst', src') = (
                        case Int.compare (dstLen, srcLen)
                         of LESS => (dstTys @ (List.tabulate (srcLen-dstLen, (fn (x) => CTy.T_Any))), srcTys)
                          | EQUAL => (dstTys, srcTys)
                          | GREATER => (dstTys, srcTys @ (List.tabulate (dstLen-srcLen, (fn (x) => CTy.T_Any)))))
                in
                    CTy.T_Tuple (b, ListPair.mapEq cleanup (dst', src'))
                end
                  | cleanup (dst as CTy.T_Fun(_, _), src as CTy.T_Fun (_, _)) =
                    if CPSTyUtil.soundMatch (dst, src)
                    then dst
                    else src
                  | cleanup (dst as CTy.T_Cont(_), src as CTy.T_Cont (_)) =
                    if CPSTyUtil.soundMatch (dst, src)
                    then dst
                    else src
                  | cleanup (dst, _) = dst
                val result = cleanup (typ, rhsType) 
            in
                SOME (result)
            end
          | typeOfRHS(C.Const (_, typ)) = SOME(typ)
          | typeOfRHS(C.Select (i, v)) = (
            (* Select is often used as a pseudo-cast, so only "change" the type if we've 
             * come across a slot that is a function type, which we might have updated.
             *)
            case CV.typeOf v
             of CTy.T_Tuple (_, tys) => (
                case List.nth (tys, i)
                 of newTy as CTy.T_Fun (_, _) => SOME (newTy)
                  | newTy as CTy.T_Cont _ => SOME (newTy)
                  | _ => NONE
                (* end case *))
              | ty => raise Fail (concat [CV.toString v,
                                          " was not of tuple type, was: ",
                                          CPSTyUtil.toString ty])
            (* end case *))
          | typeOfRHS(C.Update (_, _, _)) = NONE
          | typeOfRHS(C.AddrOf (i, v)) = (
            (* Select is often used as a pseudo-cast, so only "change" the type if we've 
             * come across a slot that is a function type, which we might have updated.
             *)
            case CV.typeOf v
             of CTy.T_Tuple (_, tys) => (
                case List.nth (tys, i)
                 of newTy as CTy.T_Fun (_, _) => SOME (CTy.T_Addr(newTy))
                  | newTy as CTy.T_Cont _ => SOME (CTy.T_Addr(newTy))
                  | _ => NONE
                (* end case *))
              | ty => raise Fail (concat [CV.toString v,
                                          " was not of tuple type, was: ",
                                          CPSTyUtil.toString ty])
            (* end case *))
          | typeOfRHS(C.Alloc (CPSTy.T_Tuple(b, tys), vars)) = let
                (*
                 * Types of items stored into an alloc are frequently wrong relative to
                 * how they're going to be used (i.e. a :enum(0) in for a ![any, any]).
                 * Only update function types.
                 *)
                fun chooseType (ty, vTy as CTy.T_Fun(_, _)) = vTy
                  | chooseType (ty, vTy as CTy.T_Cont _) = vTy
                  | chooseType (ty as CTy.T_Tuple(_,tys), vTy as CTy.T_Tuple(b,vTys)) =
                    CTy.T_Tuple(b, ListPair.map chooseType (tys, vTys))
                  | chooseType (ty, _) = ty
            in
                SOME (CPSTy.T_Tuple(b, ListPair.map chooseType (tys, (List.map CV.typeOf vars))))
            end
          | typeOfRHS(C.Alloc (_, vars)) = raise Fail "encountered an alloc that didn't originally have a tuple type."
          | typeOfRHS(C.Promote (v)) = SOME (CV.typeOf v)
          | typeOfRHS(C.Prim (prim)) = NONE (* do I need to do this one? *)
          | typeOfRHS(C.CCall (cfun, _)) = NONE
          | typeOfRHS(C.HostVProc) = NONE
          | typeOfRHS(C.VPLoad (_, _)) = NONE
          | typeOfRHS(C.VPStore (_, _, _)) = NONE
          | typeOfRHS(C.VPAddr (_, _)) = NONE
    in
        (case lhs
          of [v] => (
             case typeOfRHS (rhs)
              (* Even if we got a new type back, if the existing one is equal or more
               * specific, stick with the old one.
               *)
              of SOME(ty) => (if !closureConversionDebug
                              then print (concat["Changing ", CV.toString v,
                                                 " from: ", CPSTyUtil.toString (CV.typeOf v),
                                                 " to: ", CPSTyUtil.toString ty, "\n"])
                              else ();
                              CV.setType (v, ty))
               | NONE => ()
             (* end case *))
           | _ => ()
        (* end case *))
    end

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
        fun transformRet ret = (
            case getContKind ret
             of CALLEE_SAVE => (
                case CFACPS.valueOf ret
                 of a as CFACPS.LAMBDAS(s) => let
                        val target = hd (VSet.listItems s)
                        val newType = CV.typeOf target
                    in
                        if CPSTyUtil.equal (CV.typeOf ret, newType)
                        then ()
                        else (print (concat["Changed: ", CV.toString ret, " from ",
                                            CPSTyUtil.toString (CV.typeOf ret), " to ",
                                            CPSTyUtil.toString newType, "\n"]);
                              changed := true;
                              CV.setType (ret, newType))
                    end
                  | _ => raise Fail "blah")
              | KNOWN => transformParam ret
              | _ => ())
        and transformParam param = let
            fun getParamFunType (l, orig) =let
                val lambdas = CPS.Var.Set.listItems l
             in
                case List.length lambdas
                 of 0 => orig
                  | _ => (case getSafeCallTarget' (hd lambdas)
                           of SOME a => CV.typeOf a
                            | NONE => orig)
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
                          if !closureConversionDebug
                          then print (concat["Changed LAMBDAS(", CFACPS.valueToString a, ") for ",
                                             CV.toString param, " from: ",
                                             CPSTyUtil.toString (CV.typeOf param), " to: ",
                                             CPSTyUtil.toString newType, "\n"])
                          else ();
                          CV.setType (param, newType))
                end

              | CFACPS.TUPLE(values) => let
                    val newType = buildType (CV.typeOf param, values)
                in
                    if CPSTyUtil.equal (CV.typeOf param, newType)
                    then ()
                    else (
                          if !closureConversionDebug
                          then print (concat["Changed TUPLE for ", CV.toString param, " from: ",
                                             CPSTyUtil.toString (CV.typeOf param), " to: ",
                                             CPSTyUtil.toString newType, "\n"])
                          else ();
                          changed := true;
                          CV.setType (param, newType))
                end
              | _ => ()
        end
        and handleLambda(func as C.FB{f, params, rets, body}) = let
            val origType = CV.typeOf f
            val _ = List.app transformParam params
            val _ = List.app transformRet rets
	    val newType = case origType
                           of CTy.T_Fun _ => CTy.T_Fun (List.map CV.typeOf params, List.map CV.typeOf rets)
                            | CTy.T_Cont _ => CTy.T_Cont (List.map CV.typeOf params)
                            | _ => raise Fail (concat["Non-function type variable in a FB block: ", CV.toString f, " : ",
                                                      CPSTyUtil.toString origType])
            val _ = if CPSTyUtil.equal (origType, newType)
                    then ()
                    else (if !closureConversionDebug
                          then print (concat["Changed FB type for ", CV.toString f, " from: ",
                                             CPSTyUtil.toString origType, " to: ",
                                             CPSTyUtil.toString newType, "\n"])
                          else ();
                          changed := true;
                          CV.setType (f, newType))
	in
            walkBody (body)
	end
        and walkBody (C.Exp(_, e)) = (
            case e
             of C.Let (lhs, rhs, e) => (fixLet (lhs, rhs); walkBody (e))
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
            then (changed := false; loopParams (fb))
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
     * Convert does three things:
     * - Changes the FB param names from the "original" ones with associated
     * CFA info (so we could type-correct them!) to new ones, with the
     * copied type blasted to be equal to the one from the source
     * - Changes Apply/Throw to safe functions to take the new parameters
     * - Coerces any parameters to their required types as needed
     *)
    fun convert (env, C.MODULE{name,externs,body}) = let
        fun matchTypes (paramType::paramTypes, arg::orig, accum, final) = let
                val argType = CV.typeOf arg
            in
                if CPSTyUtil.equal (argType, paramType)
                then (print (concat[CV.toString arg, "=OK;"]); matchTypes (paramTypes, orig, arg::accum, final))
                else let
                        val typed = CV.new ("coerced", paramType)
                        val _ = if !closureConversionDebug
                                then print (concat["Coercing from: ",
                                                   CPSTyUtil.toString argType,
                                                   " to: ",
                                                   CPSTyUtil.toString paramType,
                                                   " for argument: ",
                                                   CV.toString arg, "\n"])
                                else ()
                    in
                        C.mkLet ([typed], C.Cast(paramType, arg),
                                 matchTypes (paramTypes, orig, typed::accum, final))
                    end
            end
          | matchTypes (paramTypes, [], accum, final) =
            (print "\n"; final (rev accum))
          | matchTypes (_, _, accum, final) =
            raise Fail (concat["Can't happen - call to method had mismatched arg/params."])
        fun getParamTypes f =
            case CV.typeOf f
             of CTy.T_Cont p => p
              | CTy.T_Fun (p,_) => p
              | _ => raise Fail (concat[CV.toString f, " not a fun or cont type"])
        fun convertFB (env, C.FB{f, params, rets, body}) =
            if getSafe f orelse (isCont f andalso case getContKind f of CALLEE_SAVE => true | _ => false)
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
             of C.Let (lhs, rhs, exp) => C.mkLet(lhs, convertRHS(env, lhs, rhs),
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
	            val conts' = subst'(env, conts)
                    val temp = CV.new ("anyCalleeA", CTy.T_Any)
                    fun handleCont k =
                        case getContKind k
                         of CALLEE_SAVE => (
                            case CV.kindOf k
                             of C.VK_Param _ => let
                                    val calleeParams = getCalleeParams k
                                    val nEmpties = nCalleeSaveRegs - (List.length calleeParams)
                                    val calleeParams = calleeParams @ List.tabulate (nEmpties, (fn _ => temp))
                                    val _ = print (concat[CV.toString k, " PARAM with ", Int.toString nEmpties,
                                                          " empties and calleeParams: ", String.concatWith "," (List.map CV.toString calleeParams),  "\n"])
                                in
                                    (calleeParams, nEmpties > 0)
                                end
                              | C.VK_Cont _ => let
                                    val calleeParams = getParams k
                                    val extras = List.map (fn (v,_) => v) (VMap.listItemsi calleeParams)
                                    val nEmpties = nCalleeSaveRegs - (List.length extras)
                                    val _ = print (concat[CV.toString k, " CONT with ", Int.toString nEmpties, " empties\n"])
                                in
                                    (extras @ (List.tabulate (nEmpties, (fn (_) => temp))),
                                     nEmpties > 0)
                                end
                              | _ => (print (concat[CV.toString k, " is CALLEE_SAVE w/ no params, generating 3 temps\n"]);
                                      (
                                      List.tabulate (nCalleeSaveRegs, (fn (_) => temp)),
                                      nCalleeSaveRegs > 0)))
                        | _ => ([], false)
                    val (calleeSaveParams, needsTemp) =
                        List.foldr (fn (k, (l,b)) => let
                                           val (l',b') = handleCont k
                                       in
                                           (l'@l, b' orelse b)
                                       end) ([],false) conts
                    fun maybeWrap w =
                        if needsTemp
                        then C.mkLet ([temp], C.Const(Literal.Enum(0w0), CTy.T_Any), w)
                        else w
	        in
                    case getSafeCallTarget f
                     of SOME a => let
                            val newArgsMap = getParams a
                            val newArgs = subst' (env, VMap.foldri (fn (v, _, l) => v::l) [] newArgsMap)
                            val args = args @ newArgs @ calleeSaveParams
                            val _ = if !closureConversionDebug
                                    then print (concat["Apply to safe call target through: ", CV.toString f,
                                                       " renamed to: ", CV.toString f', " safe call target named: ",
                                                       CV.toString a, "with args: ", String.concatWith "," (List.map CV.toString args),
                                                       "\n"])
                                    else ()       
                        in
                            maybeWrap (matchTypes (getParamTypes f', args, [], fn (args) =>
                                                                                  C.mkApply (f', args, conts')))
                        end
                      | NONE => let
                            val _ = if !closureConversionDebug
                                    then print (concat["Apply to unsafe call target through: ", CV.toString f,
                                                       " renamed to: ", CV.toString f', "with args: ",
                                                       String.concatWith "," (List.map CV.toString args), "\n"])
                                    else ()
                            val args = args @ calleeSaveParams
                        in
                        maybeWrap (matchTypes (getParamTypes f', args, [], fn (args) =>
                                                                                  C.mkApply (f', args, conts')))
                        end
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
                                                       CV.toString a, "with args: ", String.concatWith "," (List.map CV.toString args),
                                                       "\n"])
                                    else ()       
                        in
                            matchTypes (getParamTypes k', args, [], fn (args) => C.mkThrow (k', args))
                        end
                      | NONE => let
                            val _ = if !closureConversionDebug
                                    then print (concat["Throw to unsafe call target through: ", CV.toString k,
                                                       " renamed to: ", CV.toString k', " with args: ",
                                                       String.concatWith "," (List.map CV.toString args), "\n"])
                                    else ()
                        in
                            case getContKind k
                             of CALLEE_SAVE => (
                                (*
                                 * Params will have been annotated with the callee-save params.
                                 * Let-bound continuations are just "unused" thunks introduced in earlier
                                 * optimizations.
                                 * Otherwise, it's the name of an FB itself and we need to check that
                                 * for its params.
                                 *)
                                case CV.kindOf k'
                                 of C.VK_Param _ => let
                                        val calleeParams = getCalleeParams k
                                        val nEmpties = nCalleeSaveRegs - (List.length calleeParams)
                                    in
                                        if nEmpties = 0
                                        then C.mkThrow (k', args @ calleeParams)
                                        else let
                                                val temp = CV.new ("anyCalleeT", CTy.T_Any)
                                                val args = args @ calleeParams @ (List.tabulate (nEmpties, (fn (_) => temp)))
                                            in
                                                C.mkLet ([temp], C.Const(Literal.Enum(0w0), CTy.T_Any),
                                                         matchTypes (getParamTypes k', args, [], fn (args) => C.mkThrow (k', args)))
                                            end
                                    end
                                  | C.VK_Cont _ => let
                                        val calleeParams = getParams k
                                        val extras = List.map (fn (v,_) => v) (VMap.listItemsi calleeParams)
                                        val nEmpties = nCalleeSaveRegs - (List.length extras)
                                    in
                                        if nEmpties = 0
                                        then let
                                                val args = args @ extras
                                            in
                                                matchTypes (getParamTypes k', args, [], fn (args) => C.mkThrow (k', args))
                                            end
                                        else let
                                                val temp = CV.new ("anyCalleeT", CTy.T_Any)
                                                val args = args @ extras @ (List.tabulate (nEmpties, (fn (_) => temp)))
                                            in
                                                C.mkLet ([temp], C.Const(Literal.Enum(0w0), CTy.T_Any),
                                                         matchTypes (getParamTypes k', args, [], fn (args) => C.mkThrow (k', args)))
                                            end
                                    end
                                  | _ => let
                                        val temp = CV.new ("anyCalleeT", CTy.T_Any)
                                        val args = args @ (List.tabulate (nCalleeSaveRegs, (fn (_) => temp)))
                                    in
                                        C.mkLet ([temp], C.Const(Literal.Enum(0w0), CTy.T_Any),
                                                 matchTypes (getParamTypes k', args, [], fn (args) => C.mkThrow (k', args)))
                                    end)
                              | _ => matchTypes (getParamTypes k', args, [], fn (args) => C.mkThrow (k', args))
                        end
	        end)
        and convertRHS(env, _, C.Var(vars)) = C.Var(subst'(env,vars))
          | convertRHS(env, [l], C.Cast(ty,v)) = C.Cast(CV.typeOf l,subst(env,v))
          | convertRHS(env, _, C.Select(i,v)) = C.Select(i,subst(env,v))
          | convertRHS(env, _, C.Update(i,v1,v2)) = C.Update(i,subst(env,v1),subst(env,v2))
          | convertRHS(env, _, C.AddrOf(i,v)) = C.AddrOf(i, subst(env,v))
          | convertRHS(env, _, C.Alloc(ty,vars)) = C.Alloc(ty, subst'(env,vars))
          | convertRHS(env, _, C.Promote (v)) = C.Promote(subst(env,v))
          | convertRHS(env, _, C.Prim(p)) = C.Prim(PrimUtil.map (fn x => subst(env, x)) p)
          | convertRHS(env, _, C.CCall (var, vars)) = C.CCall (var, subst'(env,vars))
          | convertRHS(env, _, C.VPLoad(off,var)) = C.VPLoad (off, subst(env,var))
          | convertRHS(env, _, C.VPStore (off, v1, v2)) = C.VPStore (off, subst(env, v1), subst(env,v2))
          | convertRHS(env, _, C.VPAddr (off, var)) = C.VPAddr (off, subst(env, var))
          | convertRHS(env, _, x) = x
    in
        C.MODULE{
        name=name, externs=externs,
        body = convertFB (VMap.empty, body)}
    end


(*
 * The first thing we want to do is CFA. Our compiler already does CFA, so I don't need
 * to implement Section 4.1 in S&A.
 * I don't have to implement 4.2 either, because we already have a function for calculating
 * the raw free variables, and the calculation of their lifetime information (stage number, fut, lut)
 * was implemented above by Lars.
 * I don't really have to do anything for 4.3 either, because that was also implemented by Lars.
 * Note however that I should change it so nCalleeSaveRegs = 0.
 * 4.4 is the main thing I have to implement.
 *)

    fun transform module =
        if !enableClosureConversion
        then let
                val _ = FreeVars.analyze module
                val _ = CFACPS.analyze module
                val funs = getSafeFuns module
                val _ = setSNs module
                val _ = updateLUTs module
                val _ = setSlots funs
                val _ = computeParams module
                val _ = reduceParams funs
                val module = addParams module
                val _ = propagateFunChanges module
                val module = convert (VMap.empty, module)
                val _ = CFACPS.clearInfo module
                val _ = FreeVars.clear module
	        val _ = CPSCensus.census module
            in
                module
            end
        else module

(******************************************************************)
(* Below here is stuff I pulled in from flat-closure-with-cfa.sml *)
(******************************************************************)

  (* return the variable of a lambda *)
    fun funVar (CPS.FB{f, ...}) = f

  (* convert from CPS types to CFG types, guided by CFA values; *)
  (* BOT values can arise in CFA from dead code. *)
    fun cvtTy (CPSTy.T_Any, _) = CFG.T_Any
      | cvtTy (CPSTy.T_Enum w, CFA.TOP) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Enum w, CFA.BOT) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Raw rTy, CFA.TOP) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Raw rTy, CFA.TUPLE _) = CFGTy.T_Raw rTy (* datatypes *)
      | cvtTy (CPSTy.T_Raw rTy, CFA.BOT) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.TOP) = 
          CFG.T_Tuple(mut, List.map cvtTyTop tys)
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.TUPLE vs) = let
          val tysLen = List.length tys
          val vsLen = List.length vs
          val vs' = if tysLen <= vsLen
                       then List.take (vs, tysLen)
                    else vs @ (List.tabulate (tysLen - vsLen, fn _ => CFA.TOP))
          in
            CFG.T_Tuple(mut, ListPair.map cvtTy (tys, vs'))
          end
      | cvtTy (CPSTy.T_Tuple(mut, tys), CFA.BOT) = 
          CFG.T_Tuple(mut, List.map cvtTyBot tys)
      | cvtTy (CPSTy.T_Addr ty, CFA.TOP) = CFG.T_Addr(cvtTyTop ty)
      | cvtTy (CPSTy.T_Addr ty, CFA.BOT) = CFG.T_Addr(cvtTyBot ty)
      | cvtTy (ty as CPSTy.T_Cont _, v) = cvtStdContTy (ty, v)
      | cvtTy (ty as CPSTy.T_Fun _, v) = cvtStdFunTy (ty, v)
      | cvtTy (CPSTy.T_CFun cproto, _) = CFGTy.T_CFun cproto
      | cvtTy (CPSTy.T_VProc, CFA.TOP) = CFGTy.T_VProc
      | cvtTy (CPSTy.T_VProc, CFA.BOT) = CFGTy.T_VProc
      | cvtTy (CPSTy.T_Deque, CFA.TOP) = CFGTy.T_Deque
      | cvtTy (CPSTy.T_Deque, CFA.BOT) = CFGTy.T_Deque
      | cvtTy (ty, v) = raise Fail(concat[
           "bogus type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtTyTop ty = cvtTy (ty, CFA.TOP)
    and cvtTyBot ty = cvtTy (ty, CFA.BOT)

  (* convert a function type to a standard-function type, guided by CFA values *)
    and cvtStdFunTy (ty, v) = CFG.T_Tuple(false, [CFG.T_Any, cvtStdFunTyAux (ty, v)])
    and cvtStdFunTyAux (ty, CFA.TOP) = cvtStdFunTyAuxStd ty
      | cvtStdFunTyAux (ty, CFA.BOT) = cvtStdFunTyAuxStd ty
      | cvtStdFunTyAux (ty, v as CFA.LAMBDAS fs) = let
          val SOME f = CPS.Var.Set.find (fn _ => true) fs
          val CPS.VK_Fun (CPS.FB {params, rets, ...}) = CPS.Var.kindOf f
          in
             if CFA.isEscaping f
                then cvtStdFunTyAuxStd ty
             else cvtStdFunTyAuxKwn (ty, (params, rets))
          end
      | cvtStdFunTyAux (ty, v) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtStdFunTyAuxStd (CPSTy.T_Fun(argTys, [retTy, exhTy])) = CFGTy.T_StdFun{
            clos = CFGTy.T_Any,
            args = List.map cvtTyTop argTys,
            ret = cvtStdContTy (retTy, CFA.TOP),
            exh = cvtStdContTy (exhTy, CFA.TOP)
          }
      | cvtStdFunTyAuxStd (CPSTy.T_Fun(argTys, [retTy])) = CFGTy.T_KnownFunc{
            clos = CFGTy.T_Any,
            args = List.map cvtTyTop argTys @ [cvtStdContTy (retTy, CFA.TOP)]
          }
      | cvtStdFunTyAuxStd (CPSTy.T_Fun(argTys, [])) = CFGTy.T_KnownFunc{
            clos = CFGTy.T_Any,
            args = List.map cvtTyTop argTys
          }
      | cvtStdFunTyAuxStd (CPSTy.T_Any) = CFGTy.T_StdFun{
            clos = CFGTy.T_Any,
            args = [CFGTy.T_Any],
            ret = cvtStdContTy (CPSTy.T_Any, CFA.TOP),
            exh = cvtStdContTy (CPSTy.T_Any, CFA.TOP)
          }
      | cvtStdFunTyAuxStd ty = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty])
    and cvtStdFunTyAuxKwn (CPSTy.T_Fun(argTys, retTys), (args, rets)) = let
          fun cvtTy' (ty, x) = cvtTy (ty, CFA.valueOf x)
          fun cvtStdContTy' (ty, x) = cvtStdContTy (ty, CFA.valueOf x)
          in
            CFGTy.T_KnownFunc{
              clos = CFGTy.T_Any,
              args = (ListPair.mapEq cvtTy' (argTys, args)) @ 
                     (ListPair.mapEq cvtStdContTy' (retTys, rets))
            }
          end
      | cvtStdFunTyAuxKwn (ty, _) = raise Fail(concat[
          "bogus function type ", CPSTyUtil.toString ty])

  (* convert a continuation type to a standard-continuation type, guided by CFA values *)
    and cvtStdContTy (ty, v) = CFG.T_OpenTuple[cvtStdContTyAux (ty, v)]
    and cvtStdContTyAux (ty, CFA.TOP) = cvtStdContTyAuxStd ty
      | cvtStdContTyAux (ty, CFA.BOT) = cvtStdContTyAuxStd ty
      | cvtStdContTyAux (ty, CFA.LAMBDAS fs) = let
          val SOME f = CPS.Var.Set.find (fn _ => true) fs
          val CPS.VK_Cont (CPS.FB {params, rets = [], ...}) = CPS.Var.kindOf f
          in
             if CFA.isEscaping f
                then cvtStdContTyAuxStd ty
             else cvtStdContTyAuxKwn (ty, params)
          end
      | cvtStdContTyAux (ty, v) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty, " : ", CFA.valueToString v])
    and cvtStdContTyAuxStd (CPSTy.T_Cont(argTys)) =
          CFGTyUtil.stdContTy(CFGTy.T_Any, List.map cvtTyTop argTys)
      | cvtStdContTyAuxStd (CPSTy.T_Any) = 
          CFGTyUtil.stdContTy(CFGTy.T_Any, [CFGTy.T_Any])
      | cvtStdContTyAuxStd ty = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty])
    and cvtStdContTyAuxKwn (CPSTy.T_Cont(argTys), args) = let
          fun cvtTy' (ty, x) = cvtTy (ty, CFA.valueOf x)
          in
            CFGTyUtil.kwnContTy(CFGTy.T_Any, ListPair.mapEq cvtTy' (argTys, args))
          end
      | cvtStdContTyAuxKwn (ty, _) = raise Fail(concat[
          "bogus continuation type ", CPSTyUtil.toString ty])

    fun cvtTyOfVar x =
       ((cvtTy (CPS.Var.typeOf x, CFA.valueOf x))
        handle Fail s => raise Fail(concat["cvtTyOfVar(", CPS.Var.toString x, ") ==> ", s]))
    val cvtTyOfVar = fn x => let
          val ty = CPS.Var.typeOf x
          val v = CFA.valueOf x
          val ty' = cvtTyOfVar x
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "cvtTyOfVar(", CPS.Var.toString x, " : ",
                "typeOf => ", CPSTyUtil.toString ty, " ; ",
                "valueOf => ", CFA.valueToString v, ") => ", CFGTyUtil.toString ty', "\n"])
              else ();
            ty'
          end


  (* assign labels to functions and continuations *)
    local
      val {getFn : CPS.var -> CFG.label, setFn, ...} =
            CPS.Var.newProp (fn f => raise Fail(concat["labelOf(", CPS.Var.toString f, ")"]))
    in
    fun assignLabels lambda = let
          fun assignFB (CPS.FB{f, body, ...}) = let
                val fTy = CPS.Var.typeOf f
                val fVal = CFA.valueOf f
                val lab = CFG.Label.new(CPS.Var.nameOf f, cvtStdFunTyAux(fTy, fVal))
                in
                  setFn (f, lab);
                  assignExp body
                end
          and assignKB (CPS.FB{f, body, ...}) = if ClassifyConts.isJoinCont f
		then (* this continuation will map to a block, so no label now *)
		  assignExp body
		else let
		  val fTy = CPS.Var.typeOf f
		  val fVal = CFA.valueOf f
		  val lab = CFG.Label.new(CPS.Var.nameOf f, cvtStdContTyAux(fTy, fVal))
		  in
		    setFn (f, lab);
		    assignExp body
		  end
          and assignExp (CPS.Exp(_, t)) = (case t
		 of (CPS.Let(_, _, e)) => assignExp e
		  | (CPS.Fun(fbs, e)) => (List.app assignFB fbs; assignExp e)
		  | (CPS.Cont(kb, e)) => (assignKB kb; assignExp e)
		  | (CPS.If(_, e1, e2)) => (assignExp e1; assignExp e2)
		  | (CPS.Switch(_, cases, dflt)) => (
		      List.app (assignExp o #2) cases;
		      Option.app assignExp dflt)
		  | _ => ()
		(* end case *))
          in
            assignFB lambda
          end
    val setLabel = setFn
    val labelOf = getFn
    end

    datatype loc
      = Local of CFG.var        (* bound in the current function *)
      | Global of int list      (* head to tail, the list gives the slots *)
                                (* in the nested closures *)
      | EnclFun                 (* the enclosing function (or one that shares the *)
                                (* same closure). *)
      | EnclCont		(* the enclosing continuation function *)
      | JoinCont		(* a join continuation *)
      | Extern of CFG.label	(* bound to an external variable (e.g., C function *)
      | Closure of CFG.var      (* implements Shao's & Appel's whatMap *)

  (* an envrionment for mapping from CPS variables to CFG variables.  We also
   * track the current closure.
   *)
    datatype env = E of {ep : CFG.var, env : loc VMap.map}

(* +DEBUG *)
    fun locToString (Local x) = concat["L(", CFG.Var.toString x, ")"]
      | locToString (Global ns) = let
	    val names = List.map (fn n => concat[Int.toString n, ","]) ns
	in
	    concat ("G("::names@[")"]) (* we'll see if that parses lol... *)
	end
      | locToString EnclFun = "EnclFun"
      | locToString EnclCont = "EnclCont"
      | locToString JoinCont = "JoinCont"
      | locToString (Extern lab) = concat["X(", CFG.Label.toString lab, ")"]
      | locToString (Closure x) = "Closure pointer"
    fun prEnv (E{ep, env}) = let
	  fun f (x, loc) = print(concat[
		  "\n    ", CPS.Var.toString x, " --> ", locToString loc
		])
          in
            print(concat["E{ep = ", CFG.Var.toString ep, " : ", CFGTyUtil.toString(CFG.Var.typeOf ep), "\n"]);
            print "  env = {";
            VMap.appi f env;
            print "}\n}\n"
          end
(* -DEBUG *)

    fun envPtrOf (E{ep, ...}) = ep

    fun newEnv externEnv ep = E{ep = ep, env = externEnv}

  (* create a new environment that gives a fresh name to the environment pointer *)
    fun envWithFreshEP (E{ep, env}) = E{ep = CFG.Var.copy ep, env=env}

    fun insertVar (E{ep, env}, x, x') = E{ep=ep, env=VMap.insert(env, x, x')}

    fun findVar' (E{env, ...}, x) = (case VMap.find(env, x)
          of SOME loc => SOME loc
           | NONE => NONE
          (* end case *))
    fun findVar (env, x) = (case findVar'(env, x)
          of SOME loc => loc
           | NONE => raise Fail("unbound variable " ^ CPS.Var.toString x)
          (* end case *))

  (* create a new CFG variable for a CPS variable *)
    fun newVar x = CFG.Var.new (
          CPS.Var.nameOf x,
          cvtTyOfVar x)
    val newVar = fn x => let
          val x' = newVar x
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "newVar(", CPS.Var.toString x, ") => ", CFG.Var.toString x', "\n"])
              else ();
            x'
          end

  (* return the type use to represent the environment type *)
    fun envPtrType [] = CFGTy.T_Enum 0w0
      | envPtrType tys = CFGTy.T_Tuple(false, tys)

    fun newEP ty = let
          val x' = CFG.Var.new ("ep", ty)
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "newEP(_) => ", CFG.Var.toString x', "\n"])
              else ();
            x'
          end

(*
    fun newEP [] =
      (* NOTE: T_Enum(0w0) is the correct type here, but that causes problems in CheckCFG. *)
          CFG.Var.new ("ep", CFGTy.T_Any)
      | newEP tys = CFG.Var.new ("ep", CFGTy.T_Tuple(false, tys))
*)

    fun newLocal (env, x) = let
          val x' = newVar x
          in
            (insertVar(env, x, Local x'), x')
          end
 
    fun newLocalVar (env, x, ty) = let
          val x' = CFG.Var.new (CPS.Var.nameOf x, ty)
          in
            (insertVar(env, x, Local x'), x')
          end
 
    fun newLocals (E{ep, env}, xs) = let
          fun f (x, (env, xs')) = let
                val x' = newVar x
                in
                  (VMap.insert(env, x, Local x'), x'::xs')
                end
          val (env, xs) = List.foldl f (env, []) xs
          in
            (E{ep=ep, env=env}, List.rev xs)
          end
 
    fun bindLabel lab = let
          val labVar = CFG.Var.new(CFG.Label.nameOf lab, CFG.Label.typeOf lab)
          in
            (CFG.mkLabel(labVar, lab), labVar)
          end
    val bindLabel = fn lab => let
          val (bind, x') = bindLabel lab
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "bindLabel(", CFG.Label.toString lab, ") => ", CFG.Var.toString x', "\n"])
              else ();
            (bind, x')
          end

  (* lookup a CPS variable in the environment.  If it has to be fetched from
   * a closure, we introduce a new temporary for it.
   * QUESTION: should we cache the temp in the environment?
   *)
    fun lookupVar (E{ep, env}, x) = (case VMap.find(env, x)
           of SOME(Local x') => ([], x')
(*            | SOME(Global i) => let (* fetch from closure *)
 *               val tmp = newVar x
 *               in
 *                 ([CFG.mkSelect(tmp, i, ep)], tmp)
 *               end
 *)
	    | SOME(Global ns) => let (* fetch from recursive closures *)
		  fun mkBinds (n, []) = let
		      val epTy = CFG.Var.typeOf ep
		      val ty = CFGTyUtil.select(epTy, n)
		      val tmp = newEP ty
		      in
		      [CFG.mkSelect(tmp, n, ep)]
		      end
		    | mkBinds (n, (x as CFG.E_Select(t,_,_))::xs) = let
			  val epTy = CFG.Var.typeOf t
			  val ty = CFGTyUtil.select(epTy, n)
			  val tmp = newEP ty
		      in
			  (CFG.mkSelect(tmp, n, t))::x::xs
		      end
		  val (CFG.E_Select(_,n,t0))::binds = List.foldl mkBinds [] ns
		  val tmp = newVar x
		  val binds = (CFG.mkSelect(tmp, n, t0))::binds
	      in
		  (binds, tmp)
	      end		  
            | SOME EnclFun => let (* build <ep, cp> pair *)
                val (b, lab) = bindLabel(labelOf x)
		val ty = CFGTy.T_Tuple(false, [CFG.Var.typeOf ep, CFG.Var.typeOf lab])
                val tmp = CFG.Var.new(CPS.Var.nameOf x, ty)
                in
                  ([CFG.mkAlloc(tmp, ty, [ep, lab]), b], tmp)
                end
	    | SOME EnclCont => ([], ep)
	    | SOME JoinCont =>
		raise Fail("unexpected join continuation " ^ CPS.Var.toString x)
	    | SOME(Extern lab) => let
                val tmp = newVar x
                in
                  ([CFG.mkLabel(tmp, lab)], tmp)
                end
	    | SOME (Closure x) => ([], x)
	    (* This implements Shao's and Appel's whatMap. *)
	    (* We don't return any binds for a closure because we assume the bind was added when *)
	    (* the closure was created. *)
            | NONE => raise Fail(concat[
		  "unbound variable ", CPS.Var.toString x, "; ep = ", CFG.Var.toString ep
		])
          (* end case *))
    val lookupVar = fn (env, x) => let
          val (binds, x') = lookupVar (env, x)
          in
            if Controls.get ClosureControls.debug
              then print(concat[
                "lookupVar(_,", CPS.Var.toString x, ") => ", CFG.Var.toString x', "\n"])
              else ();
            (binds, x')
          end

    fun lookupVars (env, xs) = let
          fun lookup ([], binds, xs) = (binds, xs)
            | lookup (x::xs, binds, xs') = let
                val (b, x) = lookupVar(env, x)
                in
                  lookup (xs, b @ binds, x::xs')
                end
          in
            lookup (List.rev xs, [], [])
          end

    fun isPtr var = case (CPSTyUtil.kindOf (CPS.Var.typeOf var))
                     of CPSTy.K_UNIFORM => true
                      | CPSTy.K_BOXED => true
                      | _ => false

(******
* Here is my toolkit of Shao and Appel utilities.
*******)

    fun transClos (fbs, recs) = let
	val changed = ref false
	fun mergeFVMaps (maps) = let
	    fun mergeMaps ([], final) = final
	      | mergeMaps (m::maps, final) =
		mergeMaps (maps,
			   (VMap.foldli (fn (v, p as (fut, lut), final) =>
					    case VMap.find (final, v)
     					      of NONE => (changed := true; VMap.insert (final, v, p))
					       | SOME (fut', lut') => (
						 if (fut < fut' orelse lut > lut')
						 then (
						 changed := true;
						 VMap.insert (final, v, (Int.min (fut, fut'),
									 Int.max (lut, lut'))))
						 else final))
			    final m))
	in
	    case maps
	      of [] => VMap.empty
	       | [m] => m
	       | m::ms => mergeMaps(ms,m)
	end
	fun mergeRecCalls (recs, fMap) = (
	    case recs
	      of g::gs => (case VMap.find (fMap, g)
			     of SOME (futg, lutg) => let
				    val newMap = VMap.map (fn p => (futg, lutg)) (getFVMap g)
				    val mergedMap = mergeFVMaps([fMap, newMap])
				  in
				    mergeRecCalls (gs, mergedMap)
				  end
			      | NONE => mergeRecCalls (gs, fMap))
	       | [] => fMap)
    in
	(List.app (fn fb as C.FB{f, params, rets, body} => setFVMap (f, mergeRecCalls(recs, getFVMap f))) fbs;
	    if (!changed)
	    then (changed := false; transClos (fbs, recs))
	    else ())
    end

    (* yeah this is poor reuse of code, but I can refactor it later.
     * this is just to get a prototype working. 
     * The reason I duplicated mergeFVMaps is because one version 
     * uses a ref cell and the other doesn't, and I'm not immediately
     * sure how to work around that. *)

	fun mergeFVMaps (maps) = let
	    fun mergeMaps ([], final) = final
	      | mergeMaps (m::maps, final) =
		mergeMaps (maps,
			   (VMap.foldli (fn (v, p as (fut, lut), final) =>
					    case VMap.find (final, v)
     					      of NONE => VMap.insert (final, v, p)
					       | SOME (fut', lut') => (
						 if (fut < fut' orelse lut > lut')
						 then
						 VMap.insert (final, v, (Int.min (fut, fut'),
									 Int.max (lut, lut')))
						 else final))
			    final m))
	in
	    case maps
	      of [] => VMap.empty
	       | [m] => m
	       | m::ms => mergeMaps(ms,m)
	end

	fun getTrueFreeVars (f, recs, whatMap) = let
	    fun getClosures (v, p as (fut, lut), (cs, nonFuncs)) = (
		case CV.typeOf v
		  of CTy.T_Fun _ => ((if List.exists (fn x => CV.same(x,v)) recs
				     then cs
				     else
				       (case VMap.find (whatMap, v)
				         of SOME c => let val c0 = VMap.map (fn q => (fut, lut)) c in c0::cs end (* modulo implementation details of whatMap *)
				          | NONE => cs)), nonFuncs) (* do i really want to remove (v,p) in this case? *)
		   | _ => (cs, VMap.insert(nonFuncs, v, p)))
	    val (closures, nonFuncs) = VMap.foldli getClosures ([], VMap.empty) (getFVMap f)
	in
	    mergeFVMaps (nonFuncs::closures)
        end

	fun shareClosures (f, whereMap) = let
	    val emptyEP = newEP (CFGTy.T_Enum 0w0)
	    val emptyBind = CFG.mkConst(emptyEP, Literal.Enum 0w0, CFGTy.T_Enum 0w0)
	    val emptyEnv = newEnv VMap.empty emptyEP
	    val map = getFVMap f (* this is now the true free vars *)
	    val m = VMap.numItems map
	in
	    if m > 0 (* we don't do callee-save regs, so f should have only one slot. *)
	    then let
		    (* this is the predicate that makes the sharing safe-for-space: *)
		    fun subset(submap) = let
			fun subset0([]) = true
			  | subset0((x,loc)::xs) = (
			    case VMap.find (map, x)
			     of SOME _ => subset0(xs)
			      | NONE => false)
		    in
			subset0 (VMap.listItemsi submap)
		    end
		    fun isSubset (E{ep=ep, env=env}, b) = subset env
		    fun findBest(best, _, []) = best
		      | findBest(best, n, (E{ep=ep',env=env'}, b')::xs) = let
			val n0 = VMap.numItems env'
			in
			    if n < n0
			    then findBest((E{ep=ep',env=env'}, b'), n0, xs)
			    else findBest(best, n, xs)
			end
		    val safeClosures = List.filter isSubset whereMap
		    val (E{ep=ep,env=env}, b) = findBest((emptyEnv, emptyBind), 0, safeClosures)
		in
		    if VMap.numItems env > 1 (* rules out the empty case as well *)
		    then let
			    (* removeAll removes a list of CPS vars from a varmap and returns
			     * the maximum LUT of those removed *)
			    fun removeAll ([], closLut, map) = (closLut, map)
			      | removeAll ((x,loc)::rems, closLut, map) = let
				    val (map, (fut, lut)) = VMap.remove(map, x)
				    val closLut = Int.max(lut, closLut)
				in
				    removeAll (rems, closLut, map)
				end
			    val bestList = VMap.listItemsi env
			    val (closLut,map) = removeAll(bestList, ~1, map)
			in
			    (map, E{ep=ep,env=env}, b, closLut)
			end
		    else (map, emptyEnv, emptyBind, ~1) (* Don't waste my time if there are no reusable closures *)
		end
	    else (map, emptyEnv, emptyBind, ~1) (* if |TFV(f)| < 0 there is nothing to do *)
	end

								       
	(* note changed signature: *)
	(* TODO: get rid of whatMap argument; we have implemented whatMap as part of the
	 * env. This may change however. *)
    fun mkFunClosure externEnv (env, recs, whatMap, whereMap) = let
	val f = List.hd recs
	(* + DEBUG *)
	val _ = print (concat ["I think the raw free vars of ", CPS.Var.toString f, 
			       " are: ", freeVarsToString(getFVMap f), " in other words, "])
	val _ = prSet (FreeVars.envOfFun f)

	(* - DEBUG *)


	(* we assume that the transitive closure has already been calculated *)
	(* first we calculate the true free variables *)
 (*	val f = List.hd recs *)
	(* TODO: Fix getTrueFreeVars. *)
	(* for now I'm going to comment out getTrueFreeVars becuase *)
	(* I just want to get this to compile. *)

	(* val _ = setFVMap (f, getTrueFreeVars(f, recs, whatMap)) *)

	fun isNotRec (x,p) = (
	    case CV.typeOf x
	     of CTy.T_Fun _ => (if List.exists (fn v => CV.same(x,v)) recs
				then false
				else true)
	      | _ => true
	(* end case *))

	val noRecs = VMap.filteri isNotRec (getFVMap f)
	val _ = setFVMap (f, noRecs)

	(* then we use any preexisting closures we can share *)
	(* Assumption: shareClosures passes back a pair, where the first *)
	(* element is the fvMap with the vars that are in the shared closure  *)
	(* removed, and the second element is the shared closure. *)
	(* the shared closure is itself a pair whose first element is a CPS ep *)
	(* and whose second element is an env. *)
	(* closLUT will be -1 if no reusable closure was found. *)
	val (fvMap, sharedClos as E{ep = closEP, env = closEnv}, closBind, closLut) = shareClosures(f, whereMap)
	val _ = setFVMap (f, fvMap)
	(* + DEBUG *)
	val _ = print (concat ["I think the true free vars of ", CPS.Var.toString f, 
			       " are: \n", freeVarsToString(getFVMap f), "\n"])

	(* - DEBUG *)

	(* then we partition the vars into closures based on lut numbers *)
	fun partition (v, (futv, lutv), parts) =
	    case IntBinaryMap.find(parts, lutv)
	     of SOME vs => IntBinaryMap.insert(parts, lutv, v::vs)
	      | NONE => IntBinaryMap.insert(parts, lutv, [v])
	val parts = VMap.foldli partition IntBinaryMap.empty (getFVMap f)
	val _ = print(concat["There are ", Int.toString(IntBinaryMap.numItems parts), " partitions.\n"])
	(* now I want to fold mkArgs over each partition. *)
	(* each partition is just a list of CPS vars, all with the same lut. *)
	(* after that, if there is more than one partition, I will create a *)
	(* top-level "wrapper" closure with eps to the partitioned closures. *)
	(* Question for Lars: is there any reason why we build the closure *)
	(* with ptrs first? I assume its just for cache locality reasons. *)

	(* nb: I need to do something to guard against the following two *)
	(* lines failing if closLut = ~1, but I'll come back to that later. *)
	(* val (closBind, closEP') = lookupVar(env, closEP) *)
	(* val closMap = (fn E{env, ...} => env) sharedClos *)

	(* the second argument to concatBinds is the binding for link. *)
	(* I may not need this function after all. *)
(*	fun concatBinds((CFG.mkSelect(tmp,i,ep))::[], bs, link) =
	    (CFG.mkSelect(tmp,i,link))::bs
	  | concatBinds(b1::b2::bs,bs',link) = b1::concatBinds(b2::bs,bs',link)
*)
	(* nb: the index should not increment in mkSharedArgs. *)
	fun mkSharedArgs (x, location, (i, binds, clos, xs)) = let
	    val (b, x') = lookupVar(sharedClos, x)
	    val location = (
		case location
		 of Global is => Global (i::is)
		  | Closure x' => Closure x'
	    (* end case *))
	    val _ = print (concat ["Copying ", CFG.Var.toString x', " from reused closure.\n"])
	in
	    (i, b@binds, VMap.insert(clos, x, location), xs)
	end

	(* Actually, whatMap only needs to be a VMap of CPS function *)
	(* vars to the CFG ep of their closure. *)

	(* In the following, if x is a function var, I don't add it to the *)
	(* environment because we will never look up function vars in *)
	(* the environment: rather, we will look them up in whatMap. *)
	(* Furthermore, I don't need to add a bind for a function var *)
	(* (or, more precisely, for its ep) because that bind was already *)
	(* added when we created the closure and added it to the whatMap. *)

	(* Actually no. That's dumb. What I want to do is not use a whatMap *)
	(* at all, but rather, adjust the loc datatype and use environmets. *)

	fun mkArgs (x, (i, binds, clos, xs)) = let
(*	    val (b, clos, xs) = (
		case CV.typeOf x
		 of CTy.T_Fun _ => (if List.exists (fn v => CV.same(v,x)) recs
				     then ([], clos, xs) (* avail. in own bdy *)
				     else (
					 case VMap.find (whatMap, x)
					  of SOME ep => ([], ep::xs)
					   | NONE => raise Fail("function closure not found.")))
		  | _ => let
			val (b, x') = lookupVar(env, x)
		    in
			(b, VMap.insert( x'::xs)
		    end
	    (* end case *)) *)
	    val (b, x') = lookupVar(env, x)
(*	    val location = (
		case CV.typeOf x
		 of CTy.T_Fun _ => (Closure x')
		  | _ => (Global [i])
	    (* end case *))*)
	    val location = Global [i]
	    val _ = print (concat ["Adding binding for ", CFG.Var.toString x', "\n"])
	in
	    (i+1, b@binds, VMap.insert(clos, x, location), x'::xs)
	end
		       
	(* At some point I will have to add the new closures I create to *)
        (* the environment (and whereMap) so I can look them up later. *)

	val foundClosure = ref false

	fun mkPartitionArgs (lut, fvs) = let
	    val (i, binds, clos, cfgArgs) =
		if (lut = closLut)
		then (foundClosure := true;
		      VMap.foldli mkSharedArgs (0, [closBind], VMap.empty, [closEP]) closEnv)
		else (0, [], VMap.empty, [])
	    val (fvPtr, fvRaw) = List.partition isPtr fvs
	    val (i, binds, clos, cfgArgs) =
		List.foldl mkArgs (i, binds, clos, cfgArgs) fvPtr
	    val (_, binds, clos, cfgArgs) =
		List.foldl mkArgs (i, binds, clos, cfgArgs) fvRaw
            val cfgArgs = List.rev cfgArgs
	    val epTy = envPtrType (List.map CFG.Var.typeOf cfgArgs)
            val ep = newEP epTy
	    val _ = print (concat ["The ep is ", CFG.Var.toString ep, "\n"])
	    val ep' = newEP epTy
	    (* We must create the binding for the closure itself: *)
	    val newClosBind = (case epTy
			      of CFGTy.T_Enum _ => CFG.mkConst(ep, Literal.Enum 0w0, epTy)
			       | _ => CFG.mkAlloc(ep, epTy, cfgArgs)
			    (* end case *))
	    val bindEP = (case epTy
			   of CFGTy.T_Enum _ => CFG.mkConst(ep', Literal.Enum 0w0, epTy)
			    | _ => CFG.mkAlloc(ep', epTy, cfgArgs)
			 (* end case *))
        in
            (bindEP::binds, ep', cfgArgs, E{ep = ep, env = clos}, newClosBind)
        end

	val closures = IntBinaryMap.mapi mkPartitionArgs parts
	val closures = IntBinaryMap.listItems closures

	fun addTowhereMap ((bs, ep, cfgs, clos, closBind), whereMap) = (clos, closBind)::whereMap
	val whereMap = List.foldl addTowhereMap whereMap closures

	(* Make sure the shared closure gets added in, *)
	(* even if it has a different lut than everything else. *)

	(* nb: make sure to handle the case where there is no shared closure. *)

	val closures =
	    if (closLut > ~1 andalso not (!foundClosure))
	    then let
(*		    fun mkClosBinds (x, loc, binds) = let
			val (b, x') = lookupVar(sharedClos, x)
			(* val bind = concatBinds(b, closBind, closEP') *)
		    in
			b@binds
		    end
		    val binds = VMap.foldli mkClosBinds [] closEnv
		    (* But see comment up above. I think in retrospect I don't even need to *)
		    (* compute any binds here and I could just return the [] for binds. *) *)
		    val _ = print (concat ["Entering closure if with closEP = ", CFG.Var.toString closEP, "\n"])
		    val closurePart =
			([closBind], closEP, [], sharedClos, closBind)
		in
		    closurePart::closures
		end
	    else closures

	(* Let's think about what exactly I want to return. I want *)
	(* to return a single list of all the binds from all the *)
	(* closures, which of course includes their individual bindEPs; *)
	(* a single ep; a single list of the cfgArgs (which does not include *)
	(* the intermediate closure eps); and a single environment *)
	(* which is a map from CPS vars to their loc. *)

	(* Incidentally, another question I might want to ask Lars *)
	(* is what goes on under the hood with conditional function *)
	(* definitions in SML. The concept seems strange to me. *)

	(* If there is more than one closure record, I need to build *)
        (* the top-level closure. *)
	(* Actually, scratch that. I always want to build a top-level closure, *)
	(* even if there is only one partition. The reason is that there will *)
	(* probably be externs and encls and other things like that that I need *)
	(* to add to the environment, not to mention the new closure records themselves, *)
	(* and I cannot add these things directly to the closures that I intend to reuse. *)
		 
	(* N.B. I'll have to watch the semantics of what I am doing *)
	(* here closely to make sure I don't mess things up when *)
	(* dealing with the externs in externEnv. *)
	fun mkClosArgs ((bs, ep', cfgArgs', E{ep = ep, env = clos'}, closBind'),
			(i, binds, clos, cfgClosureArgs)) = let
	    fun mkClosArgs0 (x, Global is, clos) = VMap.insert(clos, x, Global (i::is))
	      | mkClosArgs0 (x, Closure x', clos) = VMap.insert(clos, x, Closure x')
	    val clos = VMap.foldli mkClosArgs0 clos clos'
	in
	    (* I shouldn't need to do anything with binds here, because I have *)
	    (* already added the binds for all the variables up above when I was *)
	    (* creating the closures. *)
	    (* I don't think the order of the binds matters here. *)
	    (i+1, bs@binds, clos, ep'::cfgClosureArgs)
	end

	val (_, binds, clos, cfgClosureArgs) =
	    List.foldl mkClosArgs (0, [], externEnv, []) closures

	val cfgClosureArgs = List.rev cfgClosureArgs
	val epTy = envPtrType (List.map CFG.Var.typeOf cfgClosureArgs)
	val ep = newEP epTy
	val ep' = newEP epTy
	val bindEP = (case epTy
	       of CFGTy.T_Enum _ => CFG.mkConst(ep', Literal.Enum 0w0, epTy)
		| _ => CFG.mkAlloc(ep', epTy, cfgClosureArgs)
	  (* end case *))
    (* Now we update whatMap: *)
(*	val clos = List.foldl (fn (x, clos) => VMap.insert(clos, x, Closure ep')) clos recs *)
	val _ = print ("Returning from mkFunCont.\n")
    in
	(* NB: The cfgClosureArgs I am returning here might not be what is expected. *)
	(* It is fine in this case since no caller ever uses that return value, *)
	(* but I think it is used in the analogous mkContClosure.  So, I should watch/revisit *)
	(* this point when working on mkContClosure. *)
	(bindEP::binds, ep', cfgClosureArgs, E{ep = ep, env = clos}, whatMap, whereMap)
    end

	(* - CARSEN *)
           

                  
  (* given a set of free CPS variables that define the environment of a function, create the
   * argument variables and bindings to build the closure and the parameter variables and
   * environment for the function's body.
   *)
(*    fun oldmkFunClosure externEnv (env, fv) = let
          fun mkArgs (x, (i, binds, clos, xs)) = let
                val (b, x') = lookupVar(env, x)
                in
                  (i+1, b@binds, VMap.insert(clos, x, Global i), x'::xs)
                end
          val (fvPtr, fvRaw) = CPS.Var.Set.partition isPtr fv
          val (i, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (0, [], externEnv, []) fvPtr
          val (_, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (i, binds, clos, cfgArgs) fvRaw
          val cfgArgs = List.rev cfgArgs
	  val epTy = envPtrType (List.map CFG.Var.typeOf cfgArgs)
          val ep = newEP epTy
	  val ep' = newEP epTy
	  val bindEP = (case epTy
		 of CFGTy.T_Enum _ => CFG.mkConst(ep', Literal.Enum 0w0, epTy)
		  | _ => CFG.mkAlloc(ep', epTy, cfgArgs)
		(* end case *))
          in
            (bindEP::binds, ep', cfgArgs, E{ep = ep, env = clos})
          end
*)
  (* given a set of free CPS variables that define the environment of a continuation, create the
   * argument variables and bindings to build the closure and the parameter variables and
   * environment for the continuation's body.
   *)
    fun mkContClosure externEnv (env, params, fv, mkContTy) = let
          val params' = List.map newVar params
          fun mkArgs (x, (i, binds, clos, xs)) = let
                val (b, x') = lookupVar(env, x)
                in
                  (i+1, b@binds, VMap.insert(clos, x, Global [i]), x'::xs)
                end
	(* the initial environment is the externs plus the parameters *)
	  val env = ListPair.foldl
		(fn (x, x', env) => VMap.insert(env, x, Local x'))
		  externEnv (params, params')
          val (fvPtr, fvRaw) = CPS.Var.Set.partition isPtr fv
          val (i, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (1, [], env, []) fvPtr
          val (_, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (i, binds, clos, cfgArgs) fvRaw
          val cfgArgs = List.rev cfgArgs
	  val epTy = envPtrType (mkContTy(CFGTy.T_Any, List.map CFG.Var.typeOf params')
                :: List.map CFG.Var.typeOf cfgArgs)
          val ep = newEP epTy
          in
            (binds, cfgArgs, E{ep = ep, env = clos}, params')
          end




    fun newConvert (m as CPS.MODULE{name, externs, body}) = let
          val blocks = ref []
        (* construct an initial environment that maps the CPS externs to CFG labels *)
          val (externs, externEnv) = let
                fun cvt (CFunctions.CFun{var, name, retTy, argTys, attrs, varArg}, (cfs, env)) = let
                      val lab = CFG.Label.new(name, cvtTyOfVar var)
                      val cf = CFG.mkCFun{var=lab, name=name, argTys=argTys, retTy=retTy, attrs=attrs, varArg=varArg}
                      in
                        (cf::cfs, VMap.insert(env, var, Extern lab))
                      end
                in
                  List.foldl cvt ([], VMap.empty) externs
                end
          val newEnv = newEnv externEnv
          val mkFunClosure = mkFunClosure externEnv
          val mkContClosure = mkContClosure externEnv
        (* convert an expression to a CFG FUNC; note that this function will convert
         * any nested functions first.
         *)
          fun cvtExp (env, params, cpsLab, lab, e, whatMap, whereMap) = let
                val () = if Controls.get ClosureControls.debug
		      then (print(concat[
			  "********************\ncvtExp: lab = ", CFG.Label.toString lab, "(",
                          CPS.Var.toString cpsLab, ")\n"
			]);
			prEnv env)
		      else ()
                (* Convert an expression into an entry block and a list of any additional blocks.
                 * A CPS function with a single conditional turns into an entry block containing
                 * the code before the conditional with a transfer that is the conditional and a
                 * list of blocks corresponding to what the true and false branches of the conditional
                 * were converted into.
                 *)
                fun cvt (env, args, CPS.Exp(_, e), stms, encl) : CFG.block * CFG.block list = let
                     fun cvtBranch (lab, e, encl) = let
                            val needsEP = ref false
                            val argEP = envPtrOf env
                            val paramEP = CFG.Var.copy argEP
                            val branchEnv = newEnv paramEP
                            fun f (x, (bEnv, args, params)) = (case findVar(env, x)
                                   of Local x' => let
                                        val (bEnv', x'') = newLocal(bEnv, x)
                                        in
                                          (bEnv', x' :: args, x'' :: params)
                                        end
                                    | Global is => (
                                        needsEP := true; 
                                        (insertVar(bEnv, x, Global is), args, params))
                                    | EnclFun => (
                                        needsEP := true;
                                        (insertVar(bEnv, x, EnclFun), args, params))
				    | EnclCont => (
                                        needsEP := true;
                                        (insertVar(bEnv, x, EnclCont), args, params))
				    | JoinCont => (bEnv, args, params)
				    | Closure x' => (insertVar(bEnv, x, Closure x'), args, params)
                                    | Extern _ => raise Fail "unexpected extern in free-var list"
                                  (* end case *))
                            val (branchEnv, args, params) =
                                  CPS.Var.Set.foldr f (branchEnv, [], []) (FV.freeVarsOfExp e)
			  (* if there are any free globals in e, then we include
			   * the environment pointer as an argument.
			   *)
                            val (args, params) = if !needsEP
                                  then (argEP :: args, paramEP :: params)
                                  else (args, params)
                            val lab' = CFG.Label.new(
                                  lab,
                                  CFGTy.T_Block{args = List.map CFG.Var.typeOf params})
                            in
                              (cvtExp (branchEnv, params, encl, lab', e, whatMap, whereMap), args)
                            end
                      in
                        case e
                         of CPS.Let(lhs, rhs, e) => let
                              val (binds, env') = cvtRHS(env, lhs, rhs)
                              in
                                cvt (env', args, e, binds @ stms, encl)
                              end
                          | CPS.Fun(fbs, e) => let
                              val (binds, env) = cvtFunc(env, fbs, whatMap, whereMap)
                              in
                                cvt (env, args, e, binds @ stms, encl)
                              end
                          | CPS.Cont(fb, e) => let
                              val (binds, env, joinBlocks) = cvtCont(env, fb, whatMap, whereMap)
                              val (start, body) = cvt (env, args, e, binds @ stms, encl)
                              val body = joinBlocks@body
                              in
                                (start, body)
                              end
                          | CPS.If(cond, e1, e2) => let
			      val (mkC, args') = CondUtil.explode cond
			      val (binds, args') = lookupVars (env, args')
                              val ((tb as CFG.BLK{lab=tlab,...}, r), targs) = cvtBranch ("then", e1, encl)
                              val ((fb as CFG.BLK{lab=flab,...}, rr), fargs) = cvtBranch ("else", e2, encl)
			      in
                                (CFG.mkBlock(lab, params, rev (binds@stms),
                                     CFG.If(mkC args', (tlab,targs), (flab,fargs))),
                                 tb::fb::r@rr)
                              end
                          | CPS.Switch(x, cases, dflt) => let
                              val (binds, x) = lookupVar(env, x)
                              val cs = List.map (fn (i, c) => (i, cvtBranch ("case", c, encl))) cases
                              val cJumps = List.map (fn (i, ((cb as CFG.BLK{lab=clab,...}, _), cargs)) =>
                                                        (i, (clab,cargs))) cs
                              val rr = List.foldr (fn ((_, ((b, bs), _)), rs) => b::bs@rs) [] cs
                              val d = case dflt
                                       of NONE => NONE
                                        | SOME e => SOME (cvtBranch ("default", e, encl))
                              val (dJump, rr) = case d
                                                 of NONE => (NONE, rr)
                                                  | SOME ((db as CFG.BLK{lab=dlab,...}, rs), dargs) =>
                                                    (SOME (dlab,dargs), db::rs@rr)
                                                        
			      in
                                (CFG.mkBlock(lab, params, rev (binds@stms),
                                     CFG.Switch(x, cJumps, dJump)),
                                 rr)
                              end
                          | CPS.Apply(f, args', rets) => let
                                val (binds, xfer) = cvtApply (env, f, args', rets)
                            in
                                (CFG.mkBlock(lab, params, rev (binds@stms), xfer), [])
                            end
                          | CPS.Throw(k, args') => let
                                val (binds, xfer) = cvtThrow (env, k, args')
                            in
                                (CFG.mkBlock(lab, params, rev (binds@stms), xfer), [])
                            end
                        (* end case *)
                      end
                in
                  cvt (env, [], e, [], cpsLab)
                end
        (* convert a CPS RHS to a list of CFG expressions, plus a new environment *)
          and cvtRHS (env, lhs, rhs) = (case (newLocals(env, lhs), rhs)
                 of ((env, lhs), CPS.Var ys) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
                        ([CFG.mkVar(lhs, ys)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Const(lit, ty)) => ([CFG.mkConst(x, lit, cvtTyTop ty)], env)
                  | ((env, [x]), CPS.Cast(ty, y)) => let
                      val (binds, y') = lookupVar(env, y)
                      in
                        ([CFG.mkCast(x, cvtTy (ty, CFA.valueOf y), y')] @ binds, env)
                      end
                  | ((env, [x]), CPS.Select(i, y)) => let
                      val (binds, y) = lookupVar(env, y)
                      in
                        ([CFG.mkSelect(x, i, y)] @ binds, env)
                      end
                  | ((env, []), CPS.Update(i, y, z)) => let
                      val (binds, y) = lookupVar(env, y)
                      val (binds', z) = lookupVar(env, z)
                      in
                        ([CFG.mkUpdate(i, y, z)] @ binds' @ binds, env)
                      end
                  | ((env, [x]), CPS.AddrOf(i, y)) => let
                      val (binds, y) = lookupVar(env, y)
                      in
                        ([CFG.mkAddrOf(x, i, y)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Alloc(ty, ys)) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
(* FIXME: should the argument be TOP here? *)
                        ([CFG.mkAlloc(x, cvtTy(ty, CFA.TOP), ys)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Promote y) => let
                      val (binds, y) = lookupVar (env, y)
                      in
                        ([CFG.mkPromote(x, y)] @ binds, env)
                      end
                  | ((env, []), CPS.Prim p) => let
                      val (mkP, args) = PrimUtil.explode p
                      val (binds, args) = lookupVars (env, args)
                      in
                        ([CFG.mkPrim0(mkP args)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Prim p) => let
                      val (mkP, args) = PrimUtil.explode p
                      val (binds, args) = lookupVars (env, args)
                      in
                        ([CFG.mkPrim(x, mkP args)] @ binds, env)
                      end
                  | ((env, res), CPS.CCall(f, args)) => let
                      val (binds, f::args) = lookupVars (env, f::args)
                      in
                        ([CFG.mkCCall(res, f, args)] @ binds, env)
                      end
                  | ((env, [vp]), CPS.HostVProc) => ([CFG.mkHostVProc(vp)], env)
                  | ((env, [x]), CPS.VPLoad(offset, vp)) => let
                      val (binds, vp) = lookupVar(env, vp)
                      in
                        ([CFG.mkVPLoad(x, offset, vp)] @ binds, env)
                      end
                  | ((env, []), CPS.VPStore(offset, vp, x)) => let
                      val (binds, [vp, x]) = lookupVars(env, [vp, x])
                      in
                        ([CFG.mkVPStore(offset, vp, x)] @ binds, env)
                      end
                  | ((env, [x]), CPS.VPAddr(offset, vp)) => let
                      val (binds, vp) = lookupVar(env, vp)
                      in
                        ([CFG.mkVPAddr(x, offset, vp)] @ binds, env)
                      end
                  | (_, rhs) => raise Fail("ill-formed RHS binding: " ^ CPSUtil.rhsToString rhs)
                (* end case *))
        (* create a standard function convention for a list of parameters *)
          and stdFuncConvention (env, args, [ret, exh]) = let
                val env = envWithFreshEP env
                val (env, args) = newLocals (env, args)
                val (env, ret) = newLocal (env, ret)
                val (env, exh) = newLocal (env, exh)
                val clos = envPtrOf env
                val conv = CFG.StdFunc{
                        clos = clos,
                        ret = ret, 
                        exh = exh
                      }
                val convTy = CFGTy.T_StdFun {
                        clos = CFG.Var.typeOf clos,
                        args = List.map CFG.Var.typeOf args,
                        ret = CFG.Var.typeOf ret,
                        exh = CFG.Var.typeOf exh
                      }
                in
                  (env, args, conv, convTy)
                end
            | stdFuncConvention (env, args, rets as [_]) = 
                kwnFuncConvention (env, args, rets)
            | stdFuncConvention (env, args, rets) =
                raise Fail "non-standard apply convention"
        (* create a known function convention for a list of parameters *)
          and kwnFuncConvention (env, args, rets) = let
                val env = envWithFreshEP env
                val (env, args) = newLocals (env, args)
                val (env, rets) = newLocals (env, rets)
                val clos = envPtrOf env
                val conv = CFG.KnownFunc{
                        clos = clos
                      }
                val convTy = CFGTy.T_KnownFunc {
                        clos = CFG.Var.typeOf clos,
                        args = List.map CFG.Var.typeOf (args @ rets)
                      }
                in
                  (env, args @ rets, conv, convTy)
                end
        (* convert bound functions *)
          and cvtFunc (env, fbs, whatMap, whereMap) = let
	      val recs = List.map funVar fbs
	      val f = List.hd recs
	      (* I need to call my own transClos function because, even though *)
	      (* FV.envOfFun returns the free vars in all recursive funs, *)
	      (* I still need to calculate the futs and luts. *)

	(* + DEBUG *)
	    val _ = print (concat ["Before transClos for func: ", CV.toString f,
				   " and its free vars are: ", freeVarsToString(getFVMap f)])
	    val _ = print(concat["There are ", Int.toString(VMap.numItems(getFVMap f)), "\n"])


(*	    val _ = prSet (FreeVars.envOfFun f)*)
(* - DEBUG *)

	      val _ = transClos (fbs, recs)
	(* + DEBUG *)
	    val _ = print (concat ["After transClos for func: ", CV.toString f,
				   " and its free vars are: ", freeVarsToString(getFVMap f)])
(*	    val _ = prSet (FreeVars.envOfFun f) *)
(* - DEBUG *)

              (* the functions share a common environment tuple *)
                val (binds, ep, clos, sharedEnv, whatMap, whereMap) =
		    mkFunClosure (env, recs, whatMap, whereMap) (* changing the sig to pass in all fbs *)
 (*                     mkFunClosure (env, FV.envOfFun(funVar(hd fbs))) *)
              (* map the names of the bound functions to EnclFun *)
                val sharedEnv = List.foldl
                      (fn (fb, env) => insertVar(env, funVar fb, EnclFun)) 
                      sharedEnv fbs
              (* convert an individual function binding; this includes creating its
               * code-pointer/environment-pointer pair and converting the function's body.
               *)
                fun cvtFB (CPS.FB{f, params, rets, body}, (binds, env)) = let
                      val (fbEnv, params, conv, convTy) = 
                            if CFA.isEscaping f
                              then stdFuncConvention (sharedEnv, params, rets)
                              else kwnFuncConvention (sharedEnv, params, rets)
                      val lab = labelOf f
                      val () = CFG.Label.setType (lab, convTy)
                      val (bindLab, labVar) = bindLabel lab
                      val (env', f') = newLocal (env, f)
		      val (env', f') = newLocalVar (env, f,
			    CFGTy.T_Tuple(false, [CFG.Var.typeOf ep, CFG.Var.typeOf labVar]))
                      val binds = CFG.mkAlloc(f', CFG.Var.typeOf f', [ep, labVar])
			    :: bindLab :: binds
                      (* convert the function itself *)
                      val (start, body) = cvtExp (fbEnv, params, f, lab, body, whatMap, whereMap)
                      in
                        finishFunc (lab, conv, start, body);
                        (binds, env')
                      end
                in
                   List.foldl cvtFB (binds, env) fbs
                end
          and finishFunc (lab, conv, start, body) = let
              val func = CFG.mkLocalFunc (lab, conv, start, body)
          in
              if Controls.get ClosureControls.debug
	      then print(concat[
			 "******************** finishFunc ", CFG.Label.toString lab, "\n"
			])
	      else ();
              blocks := func :: !blocks
          end
        (* convert a bound continuation *)
          and cvtCont (env, CPS.FB{f=k, params, body, ...}, whatMap, whereMap) =
             if ClassifyConts.isJoinCont k
		then let
		(* f is a join continuation, so we will translate it to a *
 		 * block.  We have to extend its parameters with the locally *
 		 * bound free variables. *
 		 *)
		  val needsEP = ref false
		  fun f (x, (bEnv, params)) = (case findVar(env, x)
			 of Local _ => let
			      val (bEnv', x') = newLocal(bEnv, x)
			      in
				(bEnv', x' :: params)
			      end
			  | Global is => (
			      needsEP := true;
			      (insertVar(bEnv, x, Global is), params))
			  | EnclFun => (
			      needsEP := true;
			      (insertVar(bEnv, x, EnclFun), params))
			  | EnclCont => (
			      needsEP := true;
			      (insertVar(bEnv, x, EnclCont), params))
			  | JoinCont => (bEnv, params)
			  | Closure x' => (insertVar(bEnv, x, Closure x'), params)
			  | Extern _ => raise Fail "unexpected extern in free-var list"
			(* end case *))
		  val paramEP = CFG.Var.copy (envPtrOf env)
		  val (bodyEnv, params) = newLocals (newEnv paramEP, params)
		  val (bodyEnv, params) =
			CPS.Var.Set.foldr f (bodyEnv, params) (FreeVars.envOfFun k)
		  val bodyEnv = insertVar (bodyEnv, k, JoinCont)  (* to support recursive conts *)
	       (* if there are any free globals in e, then we include *
                * the environment pointer as an argument. *)
		  val params = if !needsEP then paramEP :: params else params
		  val lab = CFG.Label.new(
			CPS.Var.nameOf k,
			CFGTy.T_Block{args = List.map CFG.Var.typeOf params})
		  val _ = setLabel (k, lab) 
                  val (start, body) = cvtExp (bodyEnv, params, k, lab, body, whatMap, whereMap)
		  in
		    ([], insertVar (env, k, JoinCont), start::body)
		  end
		else let
		  val (mkContTy, mkEntry, mkEntryTy) =
			if CFA.isEscaping k
			  then (CFGTyUtil.stdContTy, CFG.StdCont, CFGTy.T_StdCont)
			  else (CFGTyUtil.kwnContTy, CFG.KnownFunc, CFGTy.T_KnownFunc)
		  val (binds, clos, lambdaEnv, params') = 
			mkContClosure (env, params, FV.envOfFun k, mkContTy)
		  val clos' = envPtrOf lambdaEnv
		  val conv = mkEntry{
			  clos = clos'
			}
		  val convTy = mkEntryTy{
			  clos = CFG.Var.typeOf clos', 
			  args = List.map CFG.Var.typeOf params'
			}
		  val lab = labelOf k
		  val () = CFG.Label.setType (lab, convTy)
		  val (bindLab, labVar) = bindLabel lab
		  val contEnv = insertVar (lambdaEnv, k, EnclCont)  (* to support recursive conts *)
		  val clos = labVar :: clos
		  val closTy = CFGTy.T_Tuple(false, List.map CFG.Var.typeOf clos)
		  val (env', k') = newLocalVar (env, k, closTy)
		  val binds = CFG.mkAlloc(k', closTy, clos) :: bindLab :: binds
                  val (start, body) = cvtExp (contEnv, params', k, lab, body, whatMap, whereMap)
		  in
                    finishFunc (lab, conv, start, body);
		    (binds, env', [])
		  end
        (* convert an apply *)
          and cvtApply (env, f, args, rets) = (case CFA.valueOf f
                 of CFA.TOP => cvtStdApply (env, f, NONE, args, rets)
                  | CFA.BOT => cvtStdApply (env, f, NONE, args, rets)
                  | CFA.LAMBDAS gs => let
                      val SOME g = CPS.Var.Set.find (fn _ => true) gs
                      val gs = CPS.Var.Set.filter (not o CFA.isProxy) gs
                      val fTgt = if CPS.Var.Set.numItems gs = 1 
                                    then CPS.Var.Set.find (fn _ => true) gs
                                 else NONE
                      in
                        if CFA.isEscaping g
                          then cvtStdApply (env, f, fTgt, args, rets)
                          else cvtKwnApply (env, f, fTgt, args, rets)
                      end
                (* end case *))
          and cvtStdApply (env, f, fTgt, args, rets as [_, _]) = let
                val (argBinds, args) = lookupVars(env, args)
                val (retBinds, [ret, exh]) = lookupVars(env, rets)
                fun bindEP () = let
                      val (fBinds, f') = lookupVar(env, f)
                      val ep = CFG.Var.new (CPS.Var.nameOf f ^ "_ep", CFGTy.T_Any)
                      in
                        (CFG.mkSelect(ep, 0, f') :: fBinds, f', ep)
                      end
                val (binds, xfer) = let
                      val (cp, ep, binds') = (case fTgt
                             of SOME g => let
                                  val (gBind, cp) = bindLabel (labelOf g)
                                  in
                                    case findVar'(env, g)
                                     of SOME EnclFun => (cp, envPtrOf env, [gBind])
                                      | _ => let
                                          val (epBinds, _, ep) = bindEP ()
                                          in
                                            (cp, ep, gBind :: epBinds)
                                          end
                                    (* end case *)
                                  end
                              | NONE => let
                                  val (epBinds, f', ep) = bindEP ()
                                  val cp = CFG.Var.new(
                                        CFG.Var.nameOf f',
                                        CFGTyUtil.select(CFG.Var.typeOf f', 1))
                                  val cpBind = CFG.mkSelect(cp, 1, f')
                                  in
                                    (cp, ep, cpBind :: epBinds)
                                  end
                            (* end case *))
                      val xfer = CFG.StdApply{
                              f = cp,
                              clos = ep,
                              args = args,
                              ret = ret,
                              exh = exh
                            }
                      in
                        (binds', xfer)
                      end
                in
                  (binds @ retBinds @ argBinds, xfer)
                end
            | cvtStdApply (env, f, fTgt, args, rets as [_]) = 
                cvtKwnApply (env, f, fTgt, args, rets)
            | cvtStdApply (env, f, fTgt, args, rets) = 
                raise Fail "non-standard apply convention"
          and cvtKwnApply (env, f, fTgt, args, rets) = let
                val (argBinds, args) = lookupVars(env, args)
                val (retBinds, rets) = lookupVars(env, rets)
                fun bindEP () = let
                      val (fBinds, f') = lookupVar(env, f)
                      val ep = CFG.Var.new (CPS.Var.nameOf f ^ "_ep", CFGTy.T_Any)
                      in
                        (CFG.mkSelect(ep, 0, f') :: fBinds, f', ep)
                      end
                val (binds, xfer) = let
                      val (cp, ep, binds') = (case fTgt
                             of SOME g => let
                                  val (gBind, cp) = bindLabel (labelOf g)
                                  in
                                    case findVar'(env, g)
                                     of SOME EnclFun => (cp, envPtrOf env, [gBind])
                                      | _ => let
                                          val (epBinds, _, ep) = bindEP ()
                                          in
                                            (cp, ep, gBind :: epBinds)
                                          end
                                    (* end case *)
                                  end
                              | NONE => let
                                  val (epBinds, f', ep) = bindEP ()
                                  val cp = CFG.Var.new(
                                        CFG.Var.nameOf f',
                                        CFGTyUtil.select(CFG.Var.typeOf f', 1))
                                  val cpBind = CFG.mkSelect(cp, 1, f')
                                  in
                                    (cp, ep, cpBind :: epBinds)
                                  end
                            (* end case *))
                      val xfer = CFG.Apply{
                              f = cp,
                              clos = ep,
                              args = args @ rets
                            }
                      in
                        (binds', xfer)
                      end
                in
                  (binds @ retBinds @ argBinds, xfer)
                end
        (* convert a throw *)
          and cvtThrow (env, k, args) = if ClassifyConts.isJoinCont k
		then cvtJoinThrow (env, k, args)
		else (case CFA.valueOf k 
		   of CFA.TOP => cvtStdThrow (env, k, NONE, args)
		    | CFA.BOT => cvtStdThrow (env, k, NONE, args)
		    | CFA.LAMBDAS gs => let
			val SOME g = CPS.Var.Set.find (fn _ => true) gs
			val gs = CPS.Var.Set.filter (not o CFA.isProxy) gs
			val kTgt = if CPS.Var.Set.numItems gs = 1 
				      then CPS.Var.Set.find (fn _ => true) gs
				   else NONE
			in
			  if CFA.isEscaping g
			    then cvtStdThrow (env, k, kTgt, args)
			    else cvtKnownThrow (env, k, kTgt, args)
			end
		  (* end case *))
          and cvtStdThrow (env, k, kTgt, args) = let
                val (kBinds, k') = lookupVar(env, k)
                val (argBinds, args') = lookupVars(env, args)
                val cp = CFG.Var.new(CFG.Var.nameOf k',
                                     CFGTyUtil.select(CFG.Var.typeOf k', 0))
              (* if valueOf(k) = LAMBDAS {g},
               * then we can refer directly to labelOf(g)
               *)
                val bindCP = (case kTgt
                       of SOME g => CFG.mkLabel(cp, labelOf g)
                        | NONE => CFG.mkSelect(cp, 0, k')
                      (* end case *))
                val xfer = CFG.StdThrow {
                        k = cp,
                        clos = k',
                        args = args'
                     }
                in
                  (bindCP :: (argBinds @ kBinds), xfer)
                end
          and cvtKnownThrow (env, k, kTgt, args) = let
                val (kBinds, k') = lookupVar(env, k)
                val (argBinds, args') = lookupVars(env, args)
                val cp = CFG.Var.new(CFG.Var.nameOf k',
                                     CFGTyUtil.select(CFG.Var.typeOf k', 0))
              (* if valueOf(k) = LAMBDAS {g},
               * then we can refer directly to labelOf(g)
               *)
                val bindCP = (case kTgt
                       of SOME g => CFG.mkLabel(cp, labelOf g)
                        | NONE => CFG.mkSelect(cp, 0, k')
                      (* end case *))
                val xfer = CFG.Apply {
                        f = cp,
                        clos = k',
                        args = args'
                     }
                in
                  (bindCP :: (argBinds @ kBinds), xfer)
                end
	  and cvtJoinThrow (env, k, args) = let
                val (argBinds, args) = lookupVars(env, args)
		val needsEP = ref false
		fun f (x, args) = (case findVar(env, x)
		       of Local x' => x' :: args
			| Extern _ => raise Fail "unexpected extern in free-var list"
			| _ => (needsEP := true; args)
		      (* end case *))
		val args = CPS.Var.Set.foldr f args (FreeVars.envOfFun k)
	     (* if there are any free globals in e, then we include
	      * the environment pointer as an argument.
	      *)
		val args = if !needsEP then envPtrOf env :: args else args
		in
		  (argBinds, CFG.Goto(labelOf k, args))
		end
        (* create the calling convention for the module *)
          fun cvtModLambda (CPS.FB{f, params, rets, body}) = let
                val ep = CFG.Var.new ("dummyEP", CFGTy.T_Any)
                val (env, params, conv, convTy) = 
                      stdFuncConvention (E{ep = ep, env = externEnv}, params, rets)
                val lab = labelOf f
                val () = CFG.Label.setType (lab, convTy)
		val whatMap = VMap.empty
		val whereMap = []
                in
                  (lab, conv, cvtExp (env, params, f, lab, body, whatMap, whereMap))
                end
          in
            assignLabels body;
            let
		(* + S&A passes *)
		val _ = FreeVars.analyze m
		val _ = CFACPS.analyze m
		val funs = getSafeFuns m
		val _ = setSNs m
		val _ = updateLUTs m
		val _ = setSlots funs (* Do I need this? *)
		(* - S&A passes *)
                val (lab, conv, (start, body)) = cvtModLambda body
		val init = CFG.mkExportFunc(lab, conv, start, body, Atom.toString name ^ "_init")
		val _ = CFACPS.clearInfo m
		val _ = FreeVars.clear m
	    in
	      CFG.mkModule(name, externs, init::(!blocks))
	    end
          end




end
