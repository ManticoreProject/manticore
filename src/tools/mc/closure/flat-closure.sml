(* flat-closure.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a simple closure conversion algorithm.  In general, function
 * closures are represented as an environment-pointer (EP) / code-pointer (CP)
 * pair.  Mutually recursive functions share the same environment and use direct
 * calls to each other.  Continuation closures are represented as a record with
 * the first word containing the code pointer.
 *)

structure FlatClosure : sig

    val convert : CPS.module -> CFG.module

  end = struct

    structure FV = FreeVars
    structure VMap = CPS.Var.Map

  (* return the variable of a lambda *)
    fun funVar (CPS.FB{f, ...}) = f

  (* convert from CPS types to CFG types *)
    fun cvtTy (CPSTy.T_Any) = CFG.T_Any
      | cvtTy (CPSTy.T_Enum w) = CFG.T_Enum w
      | cvtTy (CPSTy.T_Raw rTy) = CFGTy.T_Raw rTy
      | cvtTy (CPSTy.T_Wrap rTy) = CFG.T_Wrap rTy
      | cvtTy (CPSTy.T_Tuple(mut, tys)) = CFG.T_Tuple(mut, List.map cvtTy tys)
      | cvtTy (CPSTy.T_Addr ty) = CFG.T_Addr(cvtTy ty)
      | cvtTy (ty as CPSTy.T_Fun(_, [])) = cvtStdContTy ty
      | cvtTy (ty as CPSTy.T_Fun _) = cvtStdFunTy ty
      | cvtTy (CPSTy.T_CFun cproto) = CFGTy.T_CFun cproto
      | cvtTy (CPSTy.T_VProc) = CFGTy.T_VProc

  (* convert a function type to a standard-function type *)
    and cvtStdFunTy ty = CFG.T_Tuple(false, [CFG.T_Any, cvtStdFunTyAux ty])
    and cvtStdFunTyAux (CPSTy.T_Fun(argTys, [retTy, exhTy])) = CFGTy.T_StdFun{
            clos = CFGTy.T_Any,
            args = List.map cvtTy argTys,
            ret = cvtStdContTy retTy,
            exh = cvtStdContTy exhTy
          }
      | cvtStdFunTyAux (CPSTy.T_Fun(argTys, [retTy])) =
	  CFGTy.T_Code(CFGTy.T_Any :: List.map cvtTy argTys @ [cvtStdContTy retTy])
      | cvtStdFunTyAux (CPSTy.T_Any) = CFGTy.T_StdFun{
            clos = CFGTy.T_Any,
            args = [CFGTy.T_Any],
            ret = cvtStdContTy CPSTy.T_Any,
            exh = cvtStdContTy CPSTy.T_Any
          }
      | cvtStdFunTyAux ty = raise Fail("bogus function type " ^ CPSTy.toString ty)

  (* convert a continuation type to a standard-continuation type *)
    and cvtStdContTy ty = CFG.T_OpenTuple[cvtStdContTyAux ty]
    and cvtStdContTyAux (CPSTy.T_Fun(argTys, [])) =
	  CFGTy.stdContTy(CFGTy.T_Any, List.map cvtTy argTys)
      | cvtStdContTyAux (CPSTy.T_Any) = CFGTy.stdContTy(CFGTy.T_Any, [CFGTy.T_Any])
      | cvtStdContTyAux ty = raise Fail("bogus continuation type " ^ CPSTy.toString ty)

  (* assign labels to functions and continuations *)
    local
      val {getFn : CPS.var -> CFG.label, setFn, ...} =
            CPS.Var.newProp (fn f => raise Fail(concat["labelOf(", CPS.Var.toString f, ")"]))
    in
    fun assignLabels lambda = let
          fun assignFB (CPS.FB{f, body, ...}) = let
                val lab = CFG.Label.new(CPS.Var.nameOf f, cvtStdFunTyAux(CPS.Var.typeOf f))
                in
                  setFn (f, lab);
                  assignExp body
                end
          and assignKB (CPS.FB{f, body, ...}) = let
                val lab = CFG.Label.new(CPS.Var.nameOf f, cvtStdContTyAux(CPS.Var.typeOf f))
                in
                  setFn (f, lab);
                  assignExp body
                end
          and assignExp (CPS.Let(_, _, e)) = assignExp e
            | assignExp (CPS.Fun(fbs, e)) = (List.app assignFB fbs; assignExp e)
            | assignExp (CPS.Cont(kb, e)) = (assignKB kb; assignExp e)
            | assignExp (CPS.If(_, e1, e2)) = (assignExp e1; assignExp e2)
            | assignExp (CPS.Switch(_, cases, dflt)) = (
                List.app (assignExp o #2) cases;
                Option.app assignExp dflt)
            | assignExp _ = ()
          in
            assignFB lambda
          end
    val labelOf = getFn
    end

    datatype loc
      = Local of CFG.var        (* bound in the current function *)
      | Global of int           (* at the ith slot of the current closure *)
      | EnclFun                 (* the enclosing function (or one that shares the *)
                                (* same closure). *)
      | EnclCont		(* the enclosing continuation function *)
      | Extern of CFG.label	(* bound to an external variable (e.g., C function *)

  (* an envrionment for mapping from CPS variables to CFG variables.  We also
   * track the current closure.
   *)
    datatype env = E of {ep : CFG.var, env : loc VMap.map}

(* +DEBUG *)
    fun locToString (Local x) = concat["L(", CFG.Var.toString x, ")"]
      | locToString (Global i) = concat["G(", Int.toString i, ")"]
      | locToString EnclFun = "EnclFun"
      | locToString EnclCont = "EnclCont"
      | locToString (Extern lab) = concat["X(", CFG.Label.toString lab, ")"]
    fun prEnv (E{ep, env}) = let
          fun f (x, loc, false) = (print(concat[", ", CPS.Var.toString x, "->", locToString loc]); false)
            | f (x, loc, true) = (print(concat[CPS.Var.toString x, "->", locToString loc]); false)
          in
            print(concat["E{ep = ", CFG.Var.toString ep, " : ", CFGTy.toString(CFG.Var.typeOf ep), "\n"]);
            print "  env = {";
            VMap.foldli f true env;
            print "}\n}\n"
          end
(* -DEBUG *)

    fun envPtrOf (E{ep, ...}) = ep

    fun newEnv externEnv ep = E{ep = ep, env = externEnv}

    fun insertVar (E{ep, env}, x, x') = E{ep=ep, env=VMap.insert(env, x, x')}

    fun findVar (E{env, ...}, x) = (case VMap.find(env, x)
          of SOME loc => loc
           | NONE => raise Fail("unbound variable " ^ CPS.Var.toString x)
          (* end case *))

  (* create a new CFG variable for a CPS variable *)
    fun newVar x = CFG.Var.new (
          CPS.Var.nameOf x,
          cvtTy(CPS.Var.typeOf x))

    fun newEP [] =
      (* NOTE: T_Enum(0w0) is the correct type here, but that causes problems in CheckCFG. *)
	  CFG.Var.new ("ep", CFGTy.T_Any)
      | newEP tys = CFG.Var.new ("ep", CFGTy.T_Tuple(false, tys))

    fun newLocal (env, x) = let
          val x' = newVar x
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

  (* lookup a CPS variable in the environment.  If it has to be fetched from
   * a closure, we introduce a new temporary for it.
   * QUESTION: should we cache the temp in the environment?
   *)
    fun lookupVar (E{ep, env}, x) = (case VMap.find(env, x)
           of SOME(Local x') => ([], x')
            | SOME(Global i) => let (* fetch from closure *)
                val tmp = newVar x
                in
                  ([CFG.mkSelect(tmp, i, ep)], tmp)
                end
            | SOME EnclFun => let (* build <ep, cp> pair *)
                val (b, lab) = bindLabel(labelOf x)
                val tmp = CFG.Var.new(
                        CPS.Var.nameOf x,
                        CFGTy.T_Tuple(false, [CFG.Var.typeOf ep, CFG.Var.typeOf lab]))
                in
                  ([CFG.mkAlloc(tmp, [ep, lab]), b], tmp)
                end
	    | SOME EnclCont => ([], ep)
	    | SOME(Extern lab) => let
                val tmp = newVar x
                in
                  ([CFG.mkLabel(tmp, lab)], tmp)
                end
            | NONE => raise Fail(concat[
		  "unbound variable ", CPS.Var.toString x, "; ep = ", CFG.Var.toString ep
		])
          (* end case *))
(*
val lookupVar = fn (environ as E{ep, env}, x) => let val (binds, x') = lookupVar(environ, x) in
print(concat["lookupVar: ", CPS.Var.toString x, " @ ", locToString(valOf(VMap.find(env, x))), " --> ", CFG.Var.toString x', "\n"]); (binds, x') end
*)

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

  (* given a set of free CPS variables that define the environment of a function, create the
   * argument variables and bindings to build the closure and the parameter variables and
   * environment for the function's body.
   *)
    fun mkFunClosure externEnv (env, fv) = let
          fun mkArgs (x, (i, binds, clos, xs)) = let
                val (b, x') = lookupVar(env, x)
                in
                  (i+1, b@binds, VMap.insert(clos, x, Global i), x'::xs)
                end
          val (_, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (0, [], externEnv, []) fv
          val cfgArgs = List.rev cfgArgs
          val ep = newEP (List.map CFG.Var.typeOf cfgArgs)
          in
            (binds, cfgArgs, E{ep = ep, env = clos})
          end

  (* given a set of free CPS variables that define the environment of a continuation, create the
   * argument variables and bindings to build the closure and the parameter variables and
   * environment for the continuation's body.
   *)
    fun mkContClosure externEnv (env, params, fv) = let
	  val params' = List.map newVar params
          fun mkArgs (x, (i, binds, clos, xs)) = let
                val (b, x') = lookupVar(env, x)
                in
                  (i+1, b@binds, VMap.insert(clos, x, Global i), x'::xs)
                end
	(* the initial environment is the externs plus the parameters *)
	  val env = ListPair.foldl
		(fn (x, x', env) => VMap.insert(env, x, Local x'))
		  externEnv (params, params')
          val (_, binds, clos, cfgArgs) =
                CPS.Var.Set.foldl mkArgs (1, [], env, []) fv
          val cfgArgs = List.rev cfgArgs
	  val ep = newEP (CFGTy.stdContTy(CFGTy.T_Any, List.map CFG.Var.typeOf params')
		  :: List.map CFG.Var.typeOf cfgArgs)
          in
            (binds, cfgArgs, E{ep = ep, env = clos}, params')
          end

    fun convert (m as CPS.MODULE{name, externs, body}) = let
          val blocks = ref []
	(* construct an initial environment that maps the CPS externs to CFG labels *)
	  val (externs, externEnv) = let
		fun cvt (CFunctions.CFun{var, name, retTy, argTys, attrs}, (cfs, env)) = let
		      val lab = CFG.Label.new(name, cvtTy(CPS.Var.typeOf var))
		      val cf = CFG.mkCFun{var=lab, name=name, argTys=argTys, retTy=retTy, attrs=attrs}
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
          fun cvtExp (env, lab, conv, e) = let
                val () =
                   if Controls.get ClosureControls.debug
                      then (print(concat["********************\ncvtExp: lab = ", CFG.Label.toString lab, "\n"]); prEnv env)
                   else ()
                fun finish (binds, xfer) = let
                      val func = CFG.mkFunc (lab, conv, List.rev binds, xfer)
                      in
                        if Controls.get ClosureControls.debug
                           then print(concat["******************** finish ", CFG.Label.toString lab, "\n"])
                        else ();
                        blocks := func :: !blocks
                      end
                fun cvt (env, e, stms) = let
                      fun branch (lab, e) = let
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
                                    | Global i => (
                                        needsEP := true; 
                                        (insertVar(bEnv, x, Global i), args, params))
                                    | EnclFun => (
                                        needsEP := true;
                                        (insertVar(bEnv, x, EnclFun), args, params))
				    | EnclCont => (
                                        needsEP := true;
                                        (insertVar(bEnv, x, EnclCont), args, params))
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
                            val lab = CFG.Label.new(
                                  lab,
                                  CFGTy.T_Code(List.map CFG.Var.typeOf params))
                            in
                              cvtExp (branchEnv, lab, CFG.Block params, e);
                              (lab, args)
                            end
                      in
                        case e
                         of CPS.Let(lhs, rhs, e) => let
                              val (binds, env') = cvtRHS(env, lhs, rhs)
                              in
                        	cvt (env', e, binds @ stms)
                              end
                          | CPS.Fun(fbs, e) => let
                            (* the functions share a common environment tuple *)
                              val (binds, clos, sharedEnv) =
				    mkFunClosure (env, FV.envOfFun(funVar(hd fbs)))
                              val ep = newEP (List.map CFG.Var.typeOf clos)
                              val bindEP = CFG.mkAlloc(ep, clos)
                            (* map the names of the bound functions to EnclFun *)
			      val sharedEnv = List.foldl
				    (fn (fb, env) => insertVar(env, funVar fb, EnclFun)) 
                                      sharedEnv fbs
                            (* convert an individual function binding; this includes creating its
                             * code-pointer/environment-pointer pair and converting the function's body.
                             *)
                              fun cvtFB (CPS.FB{f, params, rets, body}, (binds, env)) = let
                                    val lab = labelOf f
                                    val (fbEnv, conv) = stdFunConvention (sharedEnv, params, rets)
                                    val (bindLab, labVar) = bindLabel (labelOf f)
                                    val (env', f') = newLocal (env, f)
                                    val binds = CFG.mkAlloc(f', [ep, labVar]) :: bindLab :: binds
                                    in
                                    (* convert the function itself *)
                                      cvtExp (fbEnv, labelOf f, conv, body);
                                      (binds, env')
                                    end
                              val (binds, env') = List.foldl cvtFB (bindEP::binds, env) fbs
                              in
                                cvt (env', e, binds @ stms)
                              end
                          | CPS.Cont(fb, e) => let
                              val (binds, env) = cvtCont(env, fb)
                              in
                                cvt (env, e, binds @ stms)
                              end
                          | CPS.If(x, e1, e2) => let
                              val (binds, x) = lookupVar(env, x)
                              in
                                finish(binds @ stms,
				  CFG.If(x, branch("then", e1), branch("else", e2)))
                              end
                          | CPS.Switch(x, cases, dflt) => let
                              val (binds, x) = lookupVar(env, x)
                              in
                                finish(binds @ stms,
				  CFG.Switch(x,
                                    List.map (fn (i,e) => (i, branch("case", e))) cases,
                                    case dflt 
                                     of NONE => NONE
                                      | SOME e => SOME (branch("default", e))))
                              end
                          | CPS.Apply(f, args, rets) => let
                              val (argBinds, args) = lookupVars(env, args)
                              val (retBinds, rets) = lookupVars(env, rets)
			      fun bindEP () = let
                                    val (binds, f') = lookupVar(env, f)
                                    val ep = CFG.Var.new (CPS.Var.nameOf f ^ "_ep", CFGTy.T_Any)
                                    in
                                      (CFG.mkSelect(ep, 0, f') :: binds, f', ep)
                                    end
                              val (binds, xfer) = (case rets
				     of [ret, exh] => let
                                          val (cp, ep, binds') = (case CPS.Var.kindOf f
                                        	 of CPS.VK_Fun _ => let
						      val (b, cp) = bindLabel(labelOf f)
                                        	      in
                                                        case findVar(env, f)
                                                         of EnclFun => (cp, envPtrOf env, [b])
                                                          | _ => let
                                                             val (binds, _, ep) = bindEP ()
                                                             in
                                                               (cp, ep, b :: binds @ argBinds)
                                                             end
                                                        (* end case *)
                                                      end
                                                 | _ => let
                                                      val (binds, f', ep) = bindEP ()
                                                      val cp = CFG.Var.new(CFG.Var.nameOf f',
                                                              CFG.T_StdFun{
                                                                  clos = CFGTy.T_Any,
                                                                  args = List.map CFG.Var.typeOf args,
                                                                  ret = CFG.Var.typeOf ret,
                                                                  exh = CFG.Var.typeOf exh
                                                               })
                                                      val b = CFG.mkSelect(cp, 1, f')
                                                      in
                                                        (cp, ep, b :: binds @ argBinds)
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
				      | [ret] => let
                                          val (cp, ep, binds') = (case CPS.Var.kindOf f
                                        	 of CPS.VK_Fun _ => let
						      val (b, cp) = bindLabel(labelOf f)
                                        	      in
                                                        case findVar(env, f)
                                                         of EnclFun => (cp, envPtrOf env, [b])
                                                          | _ => let
                                                             val (binds, _, ep) = bindEP ()
                                                             in
                                                               (cp, ep, b :: binds @ argBinds)
                                                             end
                                                        (* end case *)
                                                      end
                                                 | _ => let
                                                      val (binds, f', ep) = bindEP ()
                                                      val cp = CFG.Var.new(CFG.Var.nameOf f',
                                                              CFG.T_Code(
								CFGTy.T_Any :: List.map CFG.Var.typeOf args
								  @ [CFG.Var.typeOf ret]))
                                                      val b = CFG.mkSelect(cp, 1, f')
                                                      in
                                                        (cp, ep, b :: binds @ argBinds)
                                                      end
						(* end case *))
					  val xfer = CFG.Apply{
                                                  f = cp,
                                                  args = ep :: args @ [ret]
                                                }
                                          in
                                            (binds', xfer)
                                          end
                                      | _ => raise Fail "non-standard apply convention"
                                    (* end case *))
                              in
                                finish (binds @ retBinds @ argBinds @ stms, xfer)
			      end
			  | CPS.Throw(k, args) => let
                              val (binds, k'::args') = lookupVars(env, k::args)
                              val (binds, xfer) = let
				    val cp = CFG.Var.new(CFG.Var.nameOf k',
					   CFGTy.selectTy(0, CFG.Var.typeOf k'))
				  (* if k has kind VK_Cont, then we can refer directly
				   * to its label
				   *)
				    val bindCP = (case CPS.Var.kindOf k
					   of CPS.VK_Cont _ => CFG.mkLabel(cp, labelOf k)
					    | _ => CFG.mkSelect(cp, 0, k')
					  (* end case *))
                                    val xfer = CFG.StdThrow{
                                            k = cp,
                                            clos = k',
                                            args = args'
                                          }
                                    in
                                      (bindCP :: binds, xfer)
                                    end
                              in
                                finish (binds @ stms, xfer)
                              end
			(* end case *)
                      end
                in
                  cvt (env, e, [])
                end
        (* convert a CPS RHS to a list of CFG expressions, plus a new environment *)
          and cvtRHS (env, lhs, rhs) = (case (newLocals(env, lhs), rhs)
                 of ((env, lhs), CPS.Var ys) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
                        ([CFG.mkVar(lhs, ys)] @ binds, env)
                      end
		  | ((env, [x]), CPS.Const(lit, ty)) => ([CFG.mkConst(x, lit)], env)
		  | ((env, [x]), CPS.Cast(ty, y)) => let
                      val (binds, y) = lookupVar(env, y)
                      in
                        ([CFG.mkCast(x, cvtTy ty, y)] @ binds, env)
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
                  | ((env, [x]), CPS.Alloc ys) => let
                      val (binds, ys) = lookupVars (env, ys)
                      in
                        ([CFG.mkAlloc(x, ys)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Wrap y) => let
                      val (binds, y) = lookupVar (env, y)
                      in
                        ([CFG.mkWrap(x, y)] @ binds, env)
                      end
                  | ((env, [x]), CPS.Unwrap y) => let
                      val (binds, y) = lookupVar (env, y)
                      in
                        ([CFG.mkUnwrap(x, y)] @ binds, env)
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
		  | (_, rhs) => raise Fail("ill-formed RHS binding: " ^ CPSUtil.rhsToString rhs)
                (* end case *))
        (* create a standard function convention for a list of parameters *)
          and stdFunConvention (env, args, [ret, exh]) = let
                val (env, args) = newLocals (env, args)
                val (env, ret) = newLocal (env, ret)
                val (env, exh) = newLocal (env, exh)
                val conv = CFG.StdFunc{
                        clos = envPtrOf env,
                        args = args, ret = ret, exh = exh
                      }
                in
                  (env, conv)
                end
	    | stdFunConvention (env, args, [ret]) = let
                val (env, args) = newLocals (env, args)
                val (env, ret) = newLocal (env, ret)
                val conv = CFG.KnownFunc(envPtrOf env :: args @ [ret])
                in
                  (env, conv)
                end
        (* convert a bound continuation *)
          and cvtCont (env, CPS.FB{f, params, body, ...}) = let
                val (binds, clos, lambdaEnv, params') = mkContClosure (env, params, FV.envOfFun f)
                val conv = CFG.StdCont{clos = envPtrOf lambdaEnv, args = params'}
                val (bindLab, labVar) = bindLabel (labelOf f)
		val contEnv = insertVar (lambdaEnv, f, EnclCont)  (* to support recursive conts *)
                val (env', k') = newLocal (env, f)
                val binds = CFG.mkAlloc(k', labVar :: clos) :: bindLab :: binds
                in
                  cvtExp (contEnv, labelOf f, conv, body);
                  (binds, env')
                end
        (* create the calling convention for the module *)
          fun cvtModLambda (CPS.FB{f, params, rets, body}) = let
                val ep = CFG.Var.new ("dummyEP", CFGTy.T_Any)
                val (env, conv) = stdFunConvention (E{ep = ep, env = externEnv}, params, rets)
                in
                  cvtExp (env, labelOf f, conv, body)
                end
          in
            FV.analyze m;
            assignLabels body;
            cvtModLambda body;
	  (* we need to rebuild the entry function so that it has an exported lambda *)
	    let val CFG.FUNC{lab, entry, body, exit} :: r = !blocks
	    val init = CFG.mkExportFunc(lab, entry, body, exit, Atom.toString name ^ "_init")
	    in
	      CFG.mkModule(name, externs, init::r)
	    end
          end

    val convert =
       BasicControl.mkKeepPass
       {preOutput = PrintCPS.output,
        preExt = "cps",
        postOutput = PrintCFG.output {types=true},
        postExt = "cfg",
        passName = "flatClosure",
        pass = convert,
        registry = ClosureControls.registry}

  end
