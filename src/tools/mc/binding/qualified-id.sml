(* qualified-id.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Check qualified ids.
 *)

structure QualifiedId : sig

  (* the result of checking a qid *)
    datatype 'a result
      = UNQUAL of Atom.atom
	(* the path was an unqualified name *)
      | QUAL of ('a * Atom.atom)
	(* the path was a qualified name: M_1 . ... . M_n . id
	 * we return the environment for M_n and id as the result.
	 *)
      | ERROR
	(* error in checking the qid *)

    type 'a path = 'a ProgramParseTree.path

    val findTy : (BindingEnv.env * Atom.atom path) -> BindingEnv.ty_bind option
    val findVar : (BindingEnv.env * Atom.atom path) -> BindingEnv.val_bind option
    val findMod : (BindingEnv.env * Atom.atom path) -> BindingEnv.mod_bind option
    val findModEnv : (BindingEnv.env * Atom.atom path) -> BindingEnv.env option

    val findBOMVar : (BindingEnv.env * Atom.atom path) -> BindingEnv.bom_var option
    val findBOMTy : (BindingEnv.env * Atom.atom path) -> BindingEnv.bom_ty_def option
    val findBOMHLOp : (BindingEnv.env * Atom.atom path) -> BindingEnv.bom_hlop option

    val toString : (('a -> string) * 'a path) -> string

    (* returns unqualified names *)
    val unqualId : 'a path -> 'a option

    val pathId : 'a path -> 'a

  end = struct

    type 'a path = 'a ProgramParseTree.path

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
	  | find (env, [id]) = (case BindingEnv.findMod (env, id)
            of NONE => ERROR
	     | SOME (_, env) => QUAL (env, x))
	  | find (env, id :: path) = (case BindingEnv.findMod (env, id)
            of NONE => ERROR
	     | SOME (_, env) => find (env, path))
	in
            find (env, path)
        end

    fun findTy (env, path) = (case checkModPath (env, path)
        of UNQUAL x => BindingEnv.findTy(env, x)
	 | QUAL (env', x) => BindingEnv.findTy(env', x)
	 | ERROR => NONE)

    fun findVar (env, path) = (case checkModPath (env, path)
        of UNQUAL x => BindingEnv.findVar(env, x)
	 | QUAL (env', x) => BindingEnv.findVar(env', x)
	 | ERROR => NONE)

    fun findMod (env, path) = (case checkModPath (env, path)
        of UNQUAL x => Option.map #1 (BindingEnv.findMod(env, x))
	 | QUAL (env', x) => Option.map #1 (BindingEnv.findMod(env', x))
	 | ERROR => NONE)

    fun findModEnv (env, path) = (case checkModPath (env, path)
        of UNQUAL x => Option.map #2 (BindingEnv.findMod(env, x))
	 | QUAL (env', x) => Option.map #2 (BindingEnv.findMod(env', x))
	 | ERROR => NONE)

    fun findBOMVar (env, path) = (case checkModPath (env, path)
        of UNQUAL x => BindingEnv.findBOMVar(env, x)
	 | QUAL (env', x) => BindingEnv.findBOMVar(env', x)
	 | ERROR => NONE)

    fun findBOMTy (env, path) = (case checkModPath (env, path)
        of UNQUAL x => BindingEnv.findBOMTy(env, x)
	 | QUAL (env', x) => BindingEnv.findBOMTy(env', x)
	 | ERROR => NONE)

    fun findBOMHLOp (env, path) = (case checkModPath (env, path)
        of UNQUAL x => BindingEnv.findBOMHLOp(env, x)
	 | QUAL (env', x) => BindingEnv.findBOMHLOp(env', x)
	 | ERROR => NONE)

  end
