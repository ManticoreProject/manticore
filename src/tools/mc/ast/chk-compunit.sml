(* chk-compunit.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check the compilation unit.
 *)

structure ChkCompUnit : sig

  (* check a compilation unit *)
    val check : (Error.err_stream * ParseTree.program) -> AST.exp

  (* check multiple compilation units *)
    val check' : (Error.err_stream * ParseTree.program) list -> AST.exp

  end = struct

  (* convert top-level declarations into expressions *)
    fun declsToExp (decls, exp) = List.foldl declToExp exp (List.rev decls)

  (* convert a top-level declaration into an expression *)
    and declToExp (decl, exp) = (case decl
        of AST.TD_Module (info, modRef, sign, module) => moduleToExp (modRef, module, exp)
	 | AST.TD_DCon dcon => exp
	 | AST.TD_Binding binding => AST.LetExp(binding, exp))   

  (* convert a module into an expression *)
    and moduleToExp (modRef, module, exp) = (case module
        of AST.M_Id _ => exp   (* FIXME: should use some special variable representation for modules *)
	 | AST.M_Body (info, decls) => declsToExp (decls, exp))

  (* call the entry-point function for the program *)
    fun makeEntryPoint (env, entryPoint) = (case Env.findVarEnv(env, Atom.atom entryPoint)
        of SOME (Env.Var entryPoint) => 
	   (* FIXME: pass command-line args *)
	   AST.ApplyExp (AST.VarExp(entryPoint, []), AST.TupleExp [AST.TupleExp [], AST.TupleExp []], Basis.unitTy)
	 | _ => raise Fail ("error: could not find entry point "^entryPoint))

  (* check a compilation unit *)
    fun check (err, program) = let
	val (env, decls) = Typechecker.check(err, program)
	in
	    declsToExp (decls, makeEntryPoint(env, "main"))
	end

  (* check multiple compilation units *)
    fun check' programs = let
	(* typecheck the compilation units individually *)
	val env0 = Env.freshEnv(Basis.te0, Basis.ve0, NONE)
	fun f ((err, program), (env, declss)) = let
            val (env', decls) = Typechecker.check'(err, env, program)
            in
	        (env', decls :: declss)
            end
	val (env, declss) = List.foldl f (env0, []) programs
        (* flatten the top-level declarations of the compilation units *)
	val decls = List.concat (List.rev declss)
	in
	(* convert the compilation units into a single AST expression *)
	    declsToExp(decls, makeEntryPoint(env, "main"))
        end

  end (* ChkModule *)
