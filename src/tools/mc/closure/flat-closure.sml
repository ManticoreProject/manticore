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

    fun newLocals (env, xs) = let
	  fun f (x, (env, xs')) = let
		val x' = newVar x
		in
		  (VMap.insert(env, x, Local x), x'::xs')
		end
	  val (env, xs) = List.foldl f (env, []) xs
	  in
	    (env, List.rev xs)
	  end

  (* lookup a CPS variable in the environment.  If it has to be fetched from
   * a closure, we introduce a new temporary for it.
   * QUESTION: should we cache the temp in the environment?
   *)
    fun lookupVar (env, x) = (case VMap.find(env, x)
	   of SOME(Local x') => ([], x')
	    | SOME(Global i) => let
		val tmp = newVar x
		in
		  ([CFG.mkSelect(tmp, i, envPtrOf env)], tmp)
		end
	    | NONE => raise Fail("unbound variable " ^ CPS.Var.toString x)
	  (* end case *))

    fun lookupVars (env, xs) = let
	  fun lookup ([], binds, xs) = (binds, xs)
	    | lookup (x::xs) = let
		val (b, x) = lookupVar(env, x)
		in
		  lookup (xs, b::binds, x::xs)
		end
	  in
	    lookup (List.rev xs, [], [])
	  end

    fun cvtExp (env, e, stms) = (case e
	   of CPS.Let(lhs, rhs, e) => let
		val (stms', env') = cvtRHS(env, lhs, rhs)
		in
		  cvtExp (env', e, stms' @ stms)
		end
	    | CPS.Fun(fbs, e) =>
	    | CPS.Cont(fb, e) =>
	    | CPS.If(x, e1, e2) =>
	    | CPS.Switch(x, cases, dflt) =>
	    | CPS.Apply(f, args) =>
	    | CPS.Throw(k, args) =>
	  (* end case *))

    and cvtRHS (env, lhs, rhs) = (case (newLocals(env, lhs), rhs)
	   of ((env, lhs), CPS.Var ys) => (?, env)
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
		in
		  (binds @ [], env)
		end
	    | ((env, [x]), CPS.CCall(f, args)) => let
		val (binds, f::args) = lookupVars (env, f::args)
		in
		  (binds @ [CFG.mkCCall(x, f, args)], env)
		end
	  (* end case *))

    fun convert (m as CPS.MODULE lambda) = (
	  FV.analyze m;
	  ??)

  end
