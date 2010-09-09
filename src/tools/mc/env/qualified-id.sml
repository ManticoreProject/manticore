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

    val findTy : (BindingEnv.env * Atom.atom path) -> BindingEnv.type_bind option
    val findVal : (BindingEnv.env * Atom.atom path) -> BindingEnv.val_bind option
    val findMod : (BindingEnv.env * Atom.atom path) -> BindingEnv.mod_bind option
    val findModEnv : (BindingEnv.env * Atom.atom path) -> BindingEnv.env option

    val findBOMVar : (BindingEnv.env * Atom.atom path) -> BindingEnv.bom_var option
    val findBOMTy : (BindingEnv.env * Atom.atom path) -> BindingEnv.bom_ty_def option
    val findBOMHLOp : (BindingEnv.env * Atom.atom path) -> BindingEnv.bom_hlop option

  (* returns unqualified names *)
    val unqualId : 'a path -> 'a option

    val pathId : 'a path -> 'a

    val toStringList : (('a -> string) * 'a path) -> string list
    val toString : (('a -> string) * 'a path) -> string

  end = struct

    type 'a path = 'a ProgramParseTree.path

    datatype 'a result
      = UNQUAL of Atom.atom
      | QUAL of ('a * Atom.atom)
      | ERROR

    fun unqualId ({tree=([], id), span}) = SOME id
      | unqualId _ = NONE

    fun pathId ({tree=(_, id), span}) = id

    fun toStringList (ts, {tree=(path, x), span}) = List.map Atom.toString path @ [ts x]

    fun toString (ts, path) = String.concatWith "." (toStringList(ts, path))

    fun checkModPath (env, {tree=([], x), span}) = UNQUAL x
      | checkModPath (env, {tree=(path, x), span}) = let
        fun find (_, []) = ERROR
	  (* we have reached the last qualified name in the path *)
	  | find (env, [id]) = (case BindingEnv.lookupMod (env, id)
            of NONE => ERROR
	     | SOME (_, env) => QUAL (env, x))
	  | find (env, id :: path) = (case BindingEnv.lookupMod (env, id)
            of NONE => ERROR
	     | SOME (_, env) => find (env, path))
	in
            find (env, path)
        end

    fun findQid findFn (env, path : Atom.atom path) : 'a option = (
	    case checkModPath(env, path)
	     of UNQUAL x => findFn(env, x)
	      | QUAL (env', x) => findFn(env', x)
	      | ERROR => NONE
            (* end case *))

    val findBOMVar = findQid BindingEnv.findBOMVar
    val findBOMTy = findQid BindingEnv.findBOMTy
    val findBOMHLOp = findQid BindingEnv.findBOMHLOp

    fun findVal (env, path) =
	(case checkModPath (env, path)
	  of UNQUAL x => BindingEnv.lookupVal (env, x)
	   | QUAL (env', x) => BindingEnv.findVal (env', x)
	   | ERROR => NONE
	(* end case *))

    fun findTy (env, path) =
	(case checkModPath (env, path)
	  of UNQUAL x => BindingEnv.lookupTy (env, x)
	   | QUAL (env', x) => BindingEnv.findTy (env', x)
	   | ERROR => NONE
	(* end case *))

    local
	fun findMod' (env, path) =
	    (case checkModPath (env, path)
	      of UNQUAL x => BindingEnv.lookupMod (env, x)
	       | QUAL (env', x) => BindingEnv.lookupMod (env', x)
	       | ERROR => NONE
	    (* end case *))
    in
    fun findMod (env, path) = Option.map #1 (findMod' (env, path))
    fun findModEnv (env, path) = Option.map #2 (findMod' (env, path))
    end

  end
