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

    fun cvtExp (env, e, stms) = (case e
	   of CPS.Let(lhs, rhs, e) => let
		val (stm, env') = cvtRHS(env, lhs, rhs)
		in
		  cvtExp (env', e, stm::stms)
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
	    | ((env, [x]), CPS.Literal lit) => (CFG.mkLiteral(x, lit), env)
	    | ((env, [x]), CPS.Select(i, y)) => (?, env)
	    | ((env, [x]), CPS.Alloc ys) => (?, env)
	    | ((env, [x]), CPS.Wrap y) => (?, env)
	    | ((env, [x]), CPS.Unwrap y) => (?, env)
	    | ((env, [x]), CPS.Prim p) => (?, env)
	    | ((env, [x]), CPS.CCall(f, args)) => (?, env)
	  (* end case *))

    fun convert (m as CPS.MODULE lambda) = (
	  FV.analyze m;
	  ??)

  end
