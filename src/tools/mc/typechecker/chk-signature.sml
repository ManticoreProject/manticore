structure ChkSignature :> sig

  (* chk that the signature is well formed, and return an environment representing the signature *)
    val check : (Error.err_stream *  Env.module_env * ParseTree.sign) -> Env.module_env

  end = struct

    fun chkSpec loc (spec, env) = (case spec
        of PT.MarkSpec {tree, span} => chkSpec span (tree, env)
	 | PT.TypeSpec tyDecl => TypeChecker.chkTyDcl span (tyDecl, env)
	 | PT.ValSpec (x, tvs, ty) => let
           val ty = TypeChecker.chkTy(loc, env, Env.empty, ty)
	   val x' = Var.new(Atom.toString x, ty)
           in
	       Env.insertVarEnv(env, x, Env.Var x')
           end

    fun chkSpecs loc (specs, env) = List.foldl (chkSpec loc) env specs

    fun check (err, env, sign) = (case sign
          of PT.MarkSig {tree, span} => chk (span, tree)
	   | PT.ExpSig specs => chkSpecs err (specs, env)
           | PT.NameSig (id, revelations) => raise Fail "todo"
          (* end case *))

  end (* ChkSignature *)