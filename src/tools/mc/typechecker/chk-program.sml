(* chk-program.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Type check a whole program.
 *)

structure ChkProgram :> sig 

  (* check several compilation units *)
    val check : (ModuleEnv.env * (Error.err_stream * ProgramParseTree.PML2.program) list) -> AST.exp

  end = struct

    structure PPT = ProgramParseTree
    structure PT = PPT.PML2
    structure Ty = Types
    structure Env = ModuleEnv

  (* add the binding `val vSig = vMod' to the list of bindings *)
    fun addBinding (vSig, Env.Var vMod, binds) = let
	  val Ty.TyScheme(tvs, ty) = Var.typeOf vSig
          in
	    AST.ValBind(AST.VarPat vSig, ASTUtil.mkVarExp(vMod, List.map Ty.VarTy tvs)) :: binds
	  end
      | addBinding (vSig, Env.Con vMod, binds) = let
          val Ty.TyScheme(tvs, ty) = Var.typeOf vSig
          in
	    AST.ValBind(AST.VarPat vSig, AST.ConstExp(AST.DConst(vMod, List.map Ty.VarTy tvs))) :: binds
	  end

  (* construct the expression
   *   let val vSig_1 = vMod_1
   *       ...
   *       val vSig_n = vMod_n
   *   in
   *     exp
   *   end
   *)
    fun bindAll (vSigs, vMods, exp) = ASTUtil.mkLetExp(ListPair.foldl addBinding [] (vSigs, vMods), exp)

  (* pair value bindings that have the same names in the signature and module *)
    fun matchValBinds (vEnvSig, vEnvMod) = let
	  fun f ((id, Env.Var vSig, SOME vMod), (vSigs, vMods)) = (vSig :: vSigs, vMod :: vMods)
	    | f ((id, _, _), (vSigs, vMods)) = (vSigs, vMods)
          in
	    List.foldl f ([], []) (Env.matchByName(vEnvSig, vEnvMod))
          end

  (* Construct value bindings for an externally-visible signature.
   * i.e., 
   *      structure F1 = struct
   *        type t<201> = int
   *        val x<101> : t<201> = 3
   *      end
   *      
   *      structure F2 : sig
   *           type t<200>
   *           val x<100> : t<200>
   *         end = Foo
   *
   *                constructValBinds(sigOf(F2), envOf(F1), exp)    ==>
   *
   *      let val x<101> : t<201> = 3
   *      in
   *          let val x<100> : t<200> = x<101>
   *          in
   *             exp
   *          end
   *      end
   *)
    fun constructValBinds (sigEnv, modEnv, exp) = let
	val (vSigs, vMods) = matchValBinds(Env.varEnv sigEnv, Env.varEnv modEnv)
	in
	    bindAll (vSigs, vMods, exp)
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
	   of AST.M_Id (info, modRef' as AST.MOD{name, ...}) => (
	      case Env.ModuleMap.find(moduleEnv, modRef)
	       of NONE => raise Fail ("cannot find module "^Atom.toString name)
		| SOME (modEnv as Env.ModEnv{modRef, ...}, sigEnv, module) => constructValBinds(sigEnv, modEnv, exp)
              (* end case *))
	    | AST.M_Body (info, decls) => declsToExp moduleEnv (decls, exp)
	  (* end case *))

    fun chkUnit (err, env, moduleEnv, {span, tree=ptDecls}) = let
	  val (env, moduleEnv, astDecls) = ChkModule.checkTopDecls err (span, ptDecls, env, moduleEnv)
	  in
	    Overload.resolve();
	    (env, moduleEnv, astDecls)
	  end

  (* check multiple compilation units *)
    fun check (mEnv0, compUnits) = let
	val dummyModRef = AST.MOD{name=Atom.atom "dummy", id=Stamp.new(), formals=NONE, expansionOpts=ref []}
	(* typecheck the compilation units individually *)
	  fun f ((err, program), (env, moduleEnv, declss)) = let
		val (env', moduleEnv', decls) = chkUnit(err, env, moduleEnv, program)
		in
		  (env', moduleEnv', decls :: declss)
		end
	  val (env, moduleEnv, declss) = List.foldl f (mEnv0, Env.ModuleMap.empty, []) compUnits
	  val Env.ModEnv{modRef=AST.MOD{expansionOpts, ...}, ...} = env
	(* elaborate the compilation units into a single expression *)
	  val program = declsToExp moduleEnv (List.concat (List.rev declss), AST.TupleExp [])
	  in
	    AST.ExpansionOptsExp(!expansionOpts, program)
	  end

    val check = BasicControl.mkKeepPass {
            preOutput = fn _ => (),
            preExt = "ppt",
            postOutput = PrintAST.outputExp,
            postExt = "ast",
            passName = "check",
            pass = check,
            registry = TCControls.registry
          }

  end (* ChkProgram *)
