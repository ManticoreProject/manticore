(* binding-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Environment for the bound-variable check.
 *)

structure BindingEnv =
  struct

    structure PT1 = ProgramParseTree.PML1
    structure PT2 = ProgramParseTree.PML2
    structure Var = ProgramParseTree.Var

    type ty_binder = PT2.ty_binder
    type var_binder = PT2.var_binder
    type mod_binder = PT2.mod_binder
    type sig_id = PT2.sig_id

  (* value identifiers may be data constructors, variables, or overloaded variables. *)
    datatype val_bind
      = Con of var_binder
      | Var of var_binder

    structure Map = AtomMap
    type ty_env = ty_binder Map.map
    type var_env = val_bind Map.map
    type mod_env = mod_binder Map.map
    datatype env
      = Env of {
	     tyEnv    : ty_env,
	     varEnv   : var_env,
	     modEnv   : (mod_binder * env) Map.map,
	     sigEnv   : (sig_id * env) Map.map,
	     outerEnv : env option       (* enclosing module *)
           }

    fun freshEnv outerEnv = Env {
           tyEnv = Map.empty,
	   varEnv = Map.empty,
	   modEnv = Map.empty,
	   sigEnv = Map.empty,
	   outerEnv = outerEnv
         }

    fun empty outerEnv = Env {
           tyEnv = Map.empty,
	   varEnv = Map.empty,
	   modEnv = Map.empty,
	   sigEnv = Map.empty,
	   outerEnv = outerEnv
         }

    fun fromList ls = List.foldl Map.insert' Map.empty ls

    fun insertVal (Env{tyEnv, varEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	Env{tyEnv=tyEnv, varEnv=Map.insert(varEnv, id, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertMod (Env{tyEnv, varEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	Env{tyEnv=tyEnv, varEnv=varEnv, modEnv=Map.insert(modEnv, id, x), sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertTy (Env{tyEnv, varEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	Env{tyEnv=Map.insert(tyEnv, id, x), varEnv=varEnv, modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertSig (Env{tyEnv, varEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	Env{tyEnv=tyEnv, varEnv=varEnv, modEnv=modEnv, sigEnv=Map.insert(sigEnv, id, x), outerEnv=outerEnv}
    val insertDataTy = insertTy

    (* lookup a variable in the scope of the current module *)
    fun findInEnv (Env (fields as {outerEnv, ...}), select, x) = (case Map.find(select fields, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findInEnv(env, select, x))
	 (* found a value *)
	 | SOME v => SOME v)	      

    fun findTy (env, tv) = findInEnv (env, #tyEnv, tv)
    fun findVar (env, v) = findInEnv (env, #varEnv, v)
    fun findMod (env, v) = findInEnv (env, #modEnv, v)
    fun findSig (env, v) = findInEnv (env, #sigEnv, v)

  (* constrains env2 to contain only those keys that are also in env1 *)
    fun intersect (env1, env2) = Map.intersectWith (fn (x1, x2) => x2) (env1, env2)

  end
