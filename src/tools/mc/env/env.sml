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
    datatype module_env
      = ModEnv of {           (* environment for modules *)
	   modRef : AST.module_ref,
	   tyEnv : ty_env,
	   varEnv : var_env,
	   modEnv : module_env AtomMap.map,
	   sigEnv : module_env AtomMap.map,
	   outerEnv : module_env option     (* environment of the enclosing module *)
         }

  (* type realization environment *)
    structure RealizationEnv = TyCon.Map
    type realization_env = ty_def RealizationEnv.map

    structure Map = AtomMap
    val empty = AtomMap.empty
    val find = AtomMap.find
    val insert = AtomMap.insert
    val inDomain = AtomMap.inDomain
    fun fromList l = List.foldl AtomMap.insert' AtomMap.empty l

    fun varEnv (ModEnv{varEnv, ...}) = varEnv

    (* lookup a variable in the scope of the current module *)
    fun findInEnv (ModEnv (fields as {outerEnv, ...}), select, x) = (case find(select fields, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findInEnv(env, select, x))
	 (* found a value *)
	 | SOME v => SOME v)	      

    fun findTyEnv (env, tv) = findInEnv (env, #tyEnv, tv)
    fun findVarEnv (env, v) = findInEnv (env, #varEnv, v)
    fun findModEnv (env, v) = findInEnv (env, #modEnv, v)
    fun findSigEnv (env, v) = findInEnv (env, #sigEnv, v)

    fun insertTyEnv (ModEnv {modRef, tyEnv, varEnv, modEnv, sigEnv, outerEnv}, tv, x) = 
	ModEnv{modRef=modRef, tyEnv=insert (tyEnv, tv, x), varEnv=varEnv, modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertVarEnv (ModEnv {modRef, varEnv, tyEnv, modEnv, sigEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=insert (varEnv, v, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertModEnv (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=insert (modEnv, v, x), sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertSigEnv (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=modEnv, sigEnv=insert (sigEnv, v, x), outerEnv=outerEnv}

    val inDomainTyEnv = Option.isSome o findTyEnv
    val inDomainVarEnv = Option.isSome o findVarEnv

    fun union (ModEnv{modRef=s1, varEnv=ve1, tyEnv=te1, modEnv=me1, sigEnv=se1, outerEnv=oe1},  
	       ModEnv{modRef=s2, varEnv=ve2, tyEnv=te2, modEnv=me2, sigEnv=se2, outerEnv=oe2}) = 
	ModEnv{modRef=s1,
	       modEnv=AtomMap.unionWith #1 (me1, me2), 
	       tyEnv=AtomMap.unionWith #1 (te1, te2), 
	       outerEnv=oe1,
	       sigEnv=se1,
	       varEnv=AtomMap.unionWith #1 (ve1, ve2)}

    fun freshEnv (modRef, tyEnv, varEnv, outerEnv) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=empty, sigEnv=empty, outerEnv=outerEnv}

  end
