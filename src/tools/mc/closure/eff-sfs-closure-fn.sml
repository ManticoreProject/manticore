(* eff-sfs-closure.sml
 *
 * COPYRIGHT (c) The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This transformation converts from a CPS IR with free variables to a
 * version of the CPS where closures are explicit and each function has
 * no free variables (known as CLO in the Shao/Appel work).
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

  (***** controls ******)
    val enableClosureConversion = ref false
    val closureConversionDebug = ref false

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
                       if CFACPS.isEscaping f
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
                  (childMap, childMap) (getFVMap f)
          in
              setFVMap (f, newMap);
              retMap
          end
          and doExp (CPS.Exp(_, e)) = (case e 
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

    (* Assign the # of slots to functions for their free variables
     * a. Initialize S(f) = max(AVAIL_REG - arg_count(f), 0)  for known, = 1 for escaping
     * b. Iterate S(f) = min ({T(g, f) | g \in V(f)} U {S(f)})
     * c. where T(g, f) = max(1, S(g)- |FV(g)-  FV(f)|)
     *)
    fun setSlots (CPS.MODULE{body, ...}) = let
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
        val funs = List.filter (fn (f) => not(CFACPS.isEscaping f)) funs
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
    (* The params properties list the new parameters for a given KNOWN function *)
    val {setFn=setParams, peekFn=peekParams, ...} = CV.newProp (fn f => []:C.Var list)

    (* The FV count is the updated free variable count for a KNOWN function *)
    val {setFn=setFVCount, getFn=getFVCount, ...} = CV.newProp (fn f => raise Fail ((CV.toString f) ^ " is not a known function that has been processed."))

    (* env is a VMap.empty, CV.Var->CV.Var *)
    fun rename (env, x, y) = (
	(* every use of x will be replaced by a use of y *)
	  VMap.insert(env, x, y))

    (* apply a substitution to a variable *)
    fun subst (env, x) = (case VMap.find(env, x)
	   of SOME y => y
	    | NONE => x
	  (* end case *))

    (* apply a substitution to a list of variables *)
    fun subst' (env, []) = []
      | subst' (env, x::xs) = subst(env, x) :: subst'(env, xs)

    fun transformFBs (env, fbs as [fb::rest]) = ()
      | transformFBs ([fb]) = ()
    and transformExp (env, C.Exp(ppt,t)) = (case t
     of C.Let (lhs, rhs, exp) => C.mkLet(lhs, subst'(env, rhs),
                                         transformExp (env, exp))
      | C.Fun (lambdas, exp) => ()
      | C.Cont (lambda, exp) => ()
      | C.If (cond, e1, e2) => C.mkIf(CondUtil.map (fn x => subst(env, x)) cond,
		                      transformExp(env, e1),
		                      transformExp(env, e2))
      | C.Switch(x, cases, dflt) => let
	    val x = subst(env, x)
	in
	    C.mkSwitch (x,
		        List.map (fn (l, e) => (l, transformExp(env, e))) cases,
		        Option.map (fn e => transformExp(env, e)) dflt)
	end
                                             
      | C.Apply(g, args, conts) => let
	    val f = subst(env, f)
	    val args = subst'(env, args)
	    val conts = subst'(env, conts)
	in
            (* TODO *)
	    case bindingOf f
	     of C.VK_Fun(C.FB{params, rets, body, ...}) =>
		if useCntOf f = 1
		then ((* inline function that is only called once *)
		      ST.tick cntBeta;
		      markInlined f;
		      inline (env, selEnv, params@rets, body, args@conts))
		else C.mkApply(f, args, conts)
	      | _ => C.mkApply(f, args, conts)
	(* end case *)
	end
      | C.Throw(k, args) => let
	    val k = subst(env, k)
	    val args = subst'(env, args)
	in
            (* TODO *)
	    case bindingOf k
	     of C.VK_Cont(C.FB{params, body, ...}) =>
		if useCntOf k = 1
		then ((* inline continuation that is only called once *)
		      ST.tick cntBetaCont;
		      markInlined k;
		      inline (env, selEnv, params, body, args))
		else C.mkThrow(k, args)
	      | _ => C.mkThrow(k, args)
	(* end case *)
	end)
    and transformRHS(env, C.Var(vars)) = C.Var(subst'(env,vars))
      | transformRHS(env, C.Cast(ty,v)) = C.Cast(ty,subst(env,v))
      | transformRHS(env, C.Select(i,v)) = C.Select(i,subst(env,v))
      | transformRHS(env, C.Update(i,v1,v2)) = C.Update(i,subst(env,v1),subst(env,v2))
      | transformRHS(env, C.AddrOf(i,v)) = C.AddrOf(i, subst(env,var))
      | transformRHS(env, C.Alloc(ty,vars)) = C.Alloc(ty, subst'(env,vars))
      | transformRHS(env, C.Promote (v)) = C.Promote(subst(env,var))
      | transformRHS(env, C.Prim(p)) = C.Prim(PrimUtil.map (fn x => subst(env, x)) p)
      | transformRHS(env, C.CCall (var, vars)) = C.CCall (var, subst'(env,vars))
      | transformRHS(env, C.VPLoad(off,var)) = C.VPLoad (off, subst(env,var))
      | transformRHS(env, C.VPStore (off, v1, v2)) = C.VPStore (off, subst(env, v1), subst(env,v2))
      | transformRHS(env, C.VPAddr (off, var)) = C.VPAddr (off, subst(env, var))
      | transformRHS(env, x) = x
    fun transform (env, C.MODULE{name,externs,body}) = let
    in
        C.MODULE{
        name=name, externs=externs,
        body = transformExp (VMap.empty, body)}
    end
                               *)

    (* *)

    fun transform module =
        if !enableClosureConversion
        then let
                val _ = CFACPS.analyze module
                val _ = FreeVars.analyze module
                val _ = print "Setting SNs\n"
                val _ = setSNs module
                val _ = print "Setting luts\n"
                val _ = updateLUTs module
                val _ = print "Setting slot counts\n"
                val _ = setSlots module
                val _ = print "Done setting slots\n"
                        
                        
                val _ = CFACPS.clearInfo module 
                val _ = FreeVars.clear module
            in
                module
            end
        else module
end
