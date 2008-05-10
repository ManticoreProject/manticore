structure BindingEnv =
  struct

    structure PT1 = ProgramParseTree.PML1
    structure PT2 = ProgramParseTree.PML2
    structure Var = ProgramParseTree.Var

    type ty_binder = PT2.ty_binder
    type var_binder = PT2.var_binder
    type mod_binder = PT2.mod_binder
    type sig_id = PT2.sig_id

    structure AM = AtomMap
    type ty_env = ty_binder AM.map
    type var_env = var_binder AM.map
    type mod_env = mod_binder AM.map
    type sig_env = sig_id AM.map
    datatype env
      = Env of {
	     tyEnv    : ty_env,
	     varEnv   : var_env,
	     modEnv   : mod_env,
	     sigEnv  : sig_env,
	     outerEnv : env option       (* enclosing module *)
           }

    fun freshEnv outerEnv = Env {
           tyEnv = AM.empty,
	   varEnv = AM.empty,
	   modEnv = AM.empty,
	   sigEnv = AM.empty,
	   outerEnv = outerEnv
         }

(* FIXME: tie this in with the basis environment *)
    fun topLevelEnv outerEnv = Env {
           tyEnv = AM.empty,
	   varEnv = AM.empty,
	   modEnv = AM.empty,
	   sigEnv = AM.empty,
	   outerEnv = outerEnv
         }

    fun insertVal _ = raise Fail ""

    fun findMod _ = raise Fail ""
    fun findVar _ = raise Fail ""
    fun findTy _ = raise Fail ""
    fun findSig _ = raise Fail ""

  end
