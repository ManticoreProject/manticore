(* env.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Env =
  struct

    datatype ty_def = TyDef of Types.ty_scheme | TyCon of Types.tycon

  (* value identifiers may be data constructors, variables, or
   * overloaded variables.
   *)
    datatype val_bind
      = Con of AST.dcon
      | Var of AST.var
      | Overload of AST.ty_scheme * AST.var list
      | EqOp of AST.var

    type ty_env = ty_def AtomMap.map		(* TE in the semantics *)
    type tyvar_env = AST.tyvar AtomMap.map	(* TVE in the semantics *)
    type var_env = val_bind AtomMap.map		(* VE in the semantics *)
    datatype module_env = ModEnv of {           (* environment for modules *)
	       tyEnv : ty_env,
	       varEnv : var_env,
	       modEnv : module_env AtomMap.map
	     }

    val empty = AtomMap.empty
    val find = AtomMap.find
    val insert = AtomMap.insert
    val inDomain = AtomMap.inDomain
    fun fromList l = List.foldl AtomMap.insert' AtomMap.empty l

    fun findTyEnv (ModEnv {tyEnv, ...}, tv) = find (tyEnv, tv)
    fun findVarEnv (ModEnv {varEnv, ...}, v) = find (varEnv, v)
    fun findModEnv (ModEnv {modEnv, ...}, m) = find (modEnv, m)

    fun insertTyEnv (ModEnv {tyEnv, ...}, tv, x) = insert (tyEnv, tv, x)
    fun insertVarEnv (ModEnv {varEnv, ...}, v, x) = insert (varEnv, v, x)
    fun insertModEnv (ModEnv {modEnv, ...}, v, x) = insert (modEnv, v, x)

  end
