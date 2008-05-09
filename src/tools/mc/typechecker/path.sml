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

    val findTy : (Env.module_env * Atom.atom ParseTree.path) -> Env.ty_def option
    val findVar : (Env.module_env * Atom.atom ParseTree.path) -> Env.val_bind option
    val findMod : (Env.module_env * Atom.atom ParseTree.path) -> Env.module_env option

    val toString : (('a -> string) * 'a ParseTree.path) -> string

    (* returns unqualified names *)
    val unqualId : 'a ParseTree.path -> 'a option

    val pathId : 'a ParseTree.path -> 'a

  end = struct

    datatype 'a result
      = UNQUAL of Atom.atom
      | QUAL of ('a * Atom.atom)
      | ERROR

    fun unqualId ({tree=([], id), span}) = SOME id
      | unqualId _ = NONE

    fun pathId ({tree=(_, id), span}) = id

    fun toString (ts, {tree=(path, x), span}) = 
	String.concatWith "." (List.map Atom.toString path @ [ts x])

    fun checkModPath (env, {tree=([], x), span}) = UNQUAL x
      | checkModPath (env, {tree=(path, x), span}) = let
        fun find (_, []) = ERROR
	  (* we have reached the last qualified name in the path *)
	  | find (env, [id]) = (case Env.findModEnv (env, id)
            of NONE => ERROR
	     | SOME env => QUAL (env, x))
	  | find (env, id :: path) = (case Env.findModEnv (env, id)
            of NONE => ERROR
	     | SOME env => find (env, path))
	in
            find (env, path)
        end

    fun findTy (env, path) = (case checkModPath (env, path)
        of UNQUAL x => Env.findTyEnv(env, x)
	 | QUAL (env', x) => Env.findTyEnv(env', x)
	 | ERROR => NONE)

    fun findVar (env, path) = (case checkModPath (env, path)
        of UNQUAL x => Env.findVarEnv(env, x)
	 | QUAL (env', x) => Env.findVarEnv(env', x)
	 | ERROR => NONE)

    fun findMod (env, path) = (case checkModPath (env, path)
        of UNQUAL x => Env.findModEnv(env, x)
	 | QUAL (env', x) => Env.findModEnv(env', x)
	 | ERROR => NONE)

  end
