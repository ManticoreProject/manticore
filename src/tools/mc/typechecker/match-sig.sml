structure MatchSig :> sig

  (* check that the module's signature matches the given signature, and return the
   * final signature for sealing the module
   *)
    val match : {err : Error.err_stream, modEnv : Env.module_env, sigEnv : Env.module_env} -> Env.module_env

  end = struct

    fun match {err, modEnv, sigEnv} = raise Fail "todo"

  end (* MatchSig *)
