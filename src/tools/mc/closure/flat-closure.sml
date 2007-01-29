(* flat-closure.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure FlatClosure : sig

    val convert : CPS.module -> CFG.module

  end = struct

    structure FV = FreeVars
    structure VMap = CPS.Var.Map

    datatype loc
      = Local of CFG.var	(* bound in the current function *)
      | Global of int		(* at the ith slot of the current closure *)

  (* an envrionment for mapping from CPS variables to CFG variables.  We also
   * track the current closure.
   *)
    datatype env = E of {ep : CFG.var, env : loc VMap.map}

  (* create a new CFG variable for a CPS variable *)
    fun newVar x = CFG.Var.new (
	  CPS.Var.nameOf x,
	  CFG.VK_None,
	  cvtTy(CPS.Var.typeOf x))

  (* create a new environment from a list of free variables *)
    fun newEnv fvs = let
	  fun f (x, (i, env, tys)) = let
		val x' = newVar x
		in
		  (i+1, VMap.insert(env, x, Global i), CFG.Var.typeOf x')
		end
	  val (_, env, tys) = List.foldl f (0, VMap.empty, []) fvs
	  val tys = List.rev tys
	  val ep = CFG.Var.new(Atom.atom "ep", CFG.VK_None, CFGTy.T_Tuple tys)
	  in
	    E{ep = ep, env = env}
	  end

    fun newLocals (E{ep, env}, xs) = let
	  fun f (x, (env, xs')) = let
		val x' = newVar x
		in
		  (VMap.insert(env, x, Local x), x'::xs')
		end
	  val (env, xs) = List.foldl f (env, []) xs
	  in
	    (E{ep=ep, env=env}, List.rev xs)
	  end

  (* lookup a variable in the environment; return NONE if it is global, otherwise return
   * SOME of the CFG variable.
   *)
    fun findLocal (E{env, ...}, x) = (case VMap.find(env, x)
	   of NONE => raise Fail("unbound variable " ^ CPS.Var.toString x)
	    | SOME(Local x') => SOME x'
	    | _ => NONE
	  (* end case *))

  (* lookup a CPS variable in the environment.  If it has to be fetched from
   * a closure, we introduce a new temporary for it.
   * QUESTION: should we cache the temp in the environment?
   *)
    fun lookupVar (E{ep, env}, x) = (case VMap.find(env, x)
	   of SOME(Local x') => ([], x')
	    | SOME(Global i) => let
		val tmp = newVar x
		in
		  ([CFG.mkSelect(tmp, i, ep)], tmp)
		end
	    | NONE => raise Fail("unbound variable " ^ CPS.Var.toString x)
	  (* end case *))

    fun lookupVars (env, xs) = let
	  fun lookup ([], binds, xs) = (binds, xs)
	    | lookup (x::xs) = let
		val (b, x) = lookupVar(env, x)
		in
		  lookup (xs, b @ binds, x::xs)
		end
	  in
	    lookup (List.rev xs, [], [])
	  end

    fun convert (m as CPS.MODULE lambda) = let
	  val blocks = ref []
	(* convert an expression to a CFG FUNC; note that this function will convert
	 * any nested functions first.
         *)
	  fun cvtExp (env, lab, conv, e) = let
		fun finish (binds, xfer) = let
		      val func = CFG.mkFunc (lab, conv, List.rev binds, xfer)
		      in
			blocks := func :: !blocks
		      end
		fun cvt (env, e, stms) = (case e
		       of CPS.Let(lhs, rhs, e) => let
			    val (stms', env') = cvtRHS(env, lhs, rhs)
			    in
			      cvt (env', e, stms' @ stms)
			    end
			| CPS.Fun(fbs, e) =>
			| CPS.Cont(fb, e) => let
			    val (binds, env) = cvtCont(env, fb)
			    in
			      cvt (env, e, binds @ stms)
			    end
			| CPS.If(x, e1, e2) => let
			    val (binds, x) = lookupVar(env, x)
			    fun mkBranch (lab, e) = let
				  val needsEP = ref false
				  fun f (x, (args, params)) = (case findLocal(env, x)
					 of SOME x' => (x' :: args, CFG.Var.copy x' :: params)
					  | NONE => (needsEP := true; (args, params))
					(* end case *))
				  val (args, params) = List.foldr f ([], []) (freeVars e)
				(* if there are any free globals in e, then we include
				 * the environment pointer as an argument.
				 *)
				  val (args, params) = if !needsEP
					then let val E{ep, ...} = env
					  in (ep :: args, CFG.Var.copy ep :: params) end
					else (args, params)
				  val lab = CFG.Label.new(
					lab,
					CFGTy.T_Code(List.map CFG.Var.typeOf params))
				  in
				    cvtExp (newEnv, lab, CFG.Block params, e);
				    (lab, args)
				  end
			    in
			      finish(binds @ stms,
				CFG.mkIf(x, branch("then", e1), branch("else", e2)))
			    end
			| CPS.Switch(x, cases, dflt) => raise Fail "switch not supported yet"
			| CPS.Apply(f, args) => let
			    val (binds, f::args) = lookupVars(env, f::args)
			    val xfer = ??
			    in
			      finish (binds @ stms, xfer)
			    end
			| CPS.Throw(k, args) => let
			    val (binds, k::args) = lookupVars(env, k::args)
			    val xfer = ??
			    in
			      finish (binds @ stms, xfer)
			    end
		      (* end case *))
		in
		end
	(* convert a CPS RHS to a list of CFG expressions, plus a new environment *)
	  and cvtRHS (env, lhs, rhs) = (case (newLocals(env, lhs), rhs)
		 of ((env, lhs), CPS.Var ys) => let
		      val (binds, ys) = lookupVars (env, ys)
		      in
			(binds @ [CG.mkVar(lhs, ys)], env)
		      end
		  | ((env, [x]), CPS.Literal lit) => ([CFG.mkLiteral(x, lit)], env)
		  | ((env, [x]), CPS.Select(i, y)) => let
		      val (binds, y) = lookupVar(env, y)
		      in
			(binds @ [CFG.mkSelect(x, i, y)], env)
		      end
		  | ((env, [x]), CPS.Alloc ys) => let
		      val (binds, ys) = lookupVars (env, ys)
		      in
			(binds @ [CFG.mkAlloc(x, ys)], env)
		      end
		  | ((env, [x]), CPS.Wrap y) => let
		      val (binds, y) = lookupVar (env, y)
		      in
			(binds @ [CFG.mkWrap(x, y)], env)
		      end
		  | ((env, [x]), CPS.Unwrap y) => let
		      val (binds, y) = lookupVar (env, y)
		      in
			(binds @ [CFG.mkUnwrap(x, y)], env)
		      end
		  | ((env, [x]), CPS.Prim p) => let
		      val (mkP, args) = PrimUtil.explode p
		      val (binds, args) = lookupVars (env, args)
		      in
			(binds @ [CFG.mkPrim(x, mkP args)], env)
		      end
		  | ((env, [x]), CPS.CCall(f, args)) => let
		      val (binds, f::args) = lookupVars (env, f::args)
		      in
			(binds @ [CFG.mkCCall(x, f, args)], env)
		      end
		(* end case *))
	(* convert a function *)
	  and cvtFun (env, (f, params, e)) = let
		in
		  (CFG.mkAlloc(f, clos) :: binds, env)
		end
	(* convert a bound continuation *)
	  and cvtCont (env, (k, params, e)) = let
		in
		  (CFG.mkAlloc(k', clos) :: binds, env)
		end
	  in
	    FV.analyze m;
	    cvtFun (VMap.empty, lambda);
	    CFG.mkModule(!blocks)
	  end

  end
