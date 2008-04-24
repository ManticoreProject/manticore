(* chk-compunit.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check the compilation unit.
 *)

structure ChkCompUnit : sig

    val check : (Error.err_stream * ParseTree.program) -> AST.exp

  end = struct

    fun declsToExp (decls, exp) = List.foldl declToExp exp (List.rev decls)

    and declToExp (decl, exp) = (case decl
        of AST.TD_Module (info, modRef, sign, module) => moduleToExp (modRef, module, exp)
	 | AST.TD_DCon dcon => exp
	 | AST.TD_Binding binding => AST.LetExp(binding, exp))   

    and moduleToExp (modRef, module, exp) = (case module
        of AST.M_Id _ => exp   (* FIXME: should use some special variable representation for modules *)
	 | AST.M_Body (info, decls) => declsToExp (decls, exp))

    fun makeEntryPoint (env, entryPoint) = (case Env.findVarEnv(env, Atom.atom entryPoint)
        of SOME (Env.Var entryPoint) => 
	   (* FIXME: pass command-line args *)
	   AST.ApplyExp (AST.VarExp(entryPoint, []), AST.TupleExp [AST.TupleExp [], AST.TupleExp []], Basis.unitTy)
	 | _ => raise Fail ("error: could not find entry point "^entryPoint))

    fun check (err, program) = let
	val (env, decls) = Typechecker.check(err, program)
	in
	    declsToExp (decls, makeEntryPoint(env, "main"))
	end

  end (* ChkModule *)
