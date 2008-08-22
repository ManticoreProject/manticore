(* chk-program.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Type check a whole program.
 *)

structure ChkProgram :> sig 

  (* check several compilation units *)
    val check : (Error.err_stream * ProgramParseTree.PML2.program) list -> AST.exp

  end = struct

    structure PPT = ProgramParseTree
    structure PT = PPT.PML2
    structure Ty = Types
    structure Env = ModuleEnv

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, msg)

    fun bindSigIdVar (vSig, vMod, binds) =
	AST.ValBind(AST.VarPat vSig, ASTUtil.mkVarExp(vMod, [])) :: binds

    fun bindSigIdVars (sigVars, modVars, exp) = 
	ASTUtil.mkLetExp(ListPair.foldl bindSigIdVar [] (sigVars, modVars), exp)

  (* bind variable definitions in the external signature to actual definitions in the module.
   * i.e., 
   *      structure Foo = struct
   *        val foo<101> = 3
   *      end
   *      
   *      structure F : sig
   *           val foo<100> : int
   *         end                     = Foo
   *
   *                                          rebindSigVars(sigOf(F), envOf(Foo), exp)    ==>
   *      let val foo<101> = 3
   *      in
   *          let val foo<100> = foo<101>
   *          in
   *             exp
   *          end
   *      end
   *)
    fun rebindSigVars (sigEnv, modEnv, exp) = let
	val sigVarEnv = Env.varEnv sigEnv
	val modVarEnv = Env.varEnv modEnv
	fun f (id, Env.Var sigVar, (sigVars, modVars)) = (case Env.VarMap.find(modVarEnv, id)
            of SOME (Env.Var modVar) => (sigVar :: sigVars, modVar :: modVars)
	     | _ => (sigVars, modVars)
            (* end case *))
	  | f (_, _, (sigVars, modVars)) = (sigVars, modVars)
        val (sigVars, modVars) = Env.VarMap.foldli f ([], []) sigVarEnv
	in
	    bindSigIdVars (sigVars, modVars, exp)
	end

  (* convert top-level declarations into expressions *)
    fun declsToExp moduleEnv (decls, exp) = List.foldl (declToExp moduleEnv) exp (List.rev decls)

  (* convert a top-level declaration into an expression *)
    and declToExp moduleEnv (decl, exp) = (case decl
	   of AST.TD_Module(info, modRef, sign, module) => 
	      moduleToExp moduleEnv (modRef, module, exp)
	    | AST.TD_DCon dcon => exp
	    | AST.TD_Binding binding => ASTUtil.mkLetExp([binding], exp)
	  (* end case *))   

  (* convert a module into an expression *)
    and moduleToExp moduleEnv (modRef, module, exp) = (case module
	   of AST.M_Id (info, modRef' as AST.MOD{name, ...}) => 
	      (case Env.ModuleMap.find(moduleEnv, modRef)
		of NONE => raise Fail ("cannot find module "^Atom.toString name)
		 | SOME (modEnv as Env.ModEnv{modRef, ...}, sigEnv, module) => 
		   rebindSigVars(sigEnv, modEnv, exp)
              (* end case *))
	    | AST.M_Body (info, decls) => declsToExp moduleEnv (decls, exp)
	  (* end case *))

    fun chkUnit (es, env, moduleEnv, {span, tree=ptDecls}) = let
	  val _ = errStrm := es
	  val (env, moduleEnv, astDecls) = ChkModule.checkTopDecls (!errStrm) (span, ptDecls, env, moduleEnv)
	  in
	    Overload.resolve ();
	    (env, moduleEnv, astDecls)
	  end

  (* check multiple compilation units *)
    fun check programs = let
	val dummyModRef = AST.MOD{name=Atom.atom "dummy", id=Stamp.new(), formals=NONE, expansionOpts=ref []}
	(* typecheck the compilation units individually *)
	  fun f ((err, program), (env, moduleEnv, declss)) = let
		val (env', moduleEnv', decls) = chkUnit(err, env, moduleEnv, program)
		in
		  (env', moduleEnv', decls :: declss)
		end
	  val (env, moduleEnv, declss) = List.foldl f (BasisEnv.mEnv0, Env.ModuleMap.empty, []) programs
	  val Env.ModEnv{modRef=AST.MOD{expansionOpts, ...}, ...} = env
        (* flatten the top-level declarations of the compilation units *)
	  val decls = List.concat (List.rev declss)
	  in
	  (* convert the compilation units into a single AST expression *)
	    AST.ExpansionOptsExp(!expansionOpts,
		declsToExp moduleEnv (decls, AST.TupleExp []))
	  end

    val check = BasicControl.mkTracePassSimple {passName = "check", pass = check}

  end (* ChkProgram *)
