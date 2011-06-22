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
    structure U = CPSUtil
    structure CTy = CPSTy
    structure CFA = CFACPS
    structure ST = Stats

    val N = Target.availRegs

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

    fun transform module = let
    in
        module
    end
end
