(* env.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Env =
  struct

    datatype ty_def = TyDef of AST.ty_scheme | TyCon of AST.tycon

    datatype val_bind = Con of AST.dcon | Var of AST.var

    type ty_env = ty_def AtomMap.map		(* TE in the semantics *)
    type tyvar_env = AST.tyvar AtomMap.map	(* TVE in the semantics *)
    type var_env = val_bind AtomMap.map		(* VE in the semantics *)

    val empty = AtomMap.empty
    val find = AtomMap.find
    val insert = AtomMap.insert
    val inDomain = AtomMap.inDomain
    fun fromList l = List.foldl AtomMap.insert' AtomMap.empty l

  end
