(* path.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check access paths in qualified names.
 *)

structure Path : sig

  (* the result of checking a path *)
    datatype 'a result
      = UNQUAL of Atom.atom
	(* the path was an unqualified name *)
      | QUAL of ('a * Atom.atom)
	(* the path was a qualified name: M_1 . ... . M_n . id
	 * we return the environment for M_n and id as the result.
	 *)
      | ERROR
	(* error in checking the path *)

    val checkValId : (Env.env * Atom.atom ParseTree.path) -> Env.module_env result

  end = struct

    datatype result
      = UNQUAL of Atom.atom
      | QUAL of (UnitEnv.env * Atom.atom)
      | ERROR

    fun check (env, {tree=([], x), span}) = UNQUAL x
      | check (env, {tree=(path, x), span}) = let
        fun find (_, []) = ERROR
	  | find (env, [id]) = (case Env.findModEnv (env, id)
            of NONE => ERROR
	     | SOME env => QUAL (env, x))
	  | find (env, id :: path) = (case Env.findModEnv (env, id)
            of NONE => ERROR
	     | SOME env => find (env, path))
	in
            find (env, path)
        end

  end
