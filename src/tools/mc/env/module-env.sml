(* module-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Environment representation for modules.
 *)

structure ModuleEnv =
  struct

    structure VarMap = ProgramParseTree.Var.Map
    structure TyVarMap = AtomMap

    datatype ty_def = TyDef of Types.ty_scheme | TyCon of Types.tycon

  (* value identifiers may be data constructors, variables, or overloaded variables. *)
    datatype val_bind
      = Con of AST.dcon
      | Var of AST.var
      | Overload of AST.ty_scheme * AST.var list
      | EqOp of AST.var

    type ty_env = ty_def VarMap.map		(* TE in the semantics *)
    type tyvar_env = AST.tyvar AtomMap.map	(* TVE in the semantics *)
    type var_env = val_bind VarMap.map		(* VE in the semantics *)

  (* module environment that mirrors the actual nesting structure of a module *)
    datatype env
      = ModEnv of {
	   modRef : AST.module_ref,         (* module identification *)
	   tyEnv : ty_env,                  (* type definitions *)
	   varEnv : var_env,                (* variable definitions *)
	   modEnv : env VarMap.map,         (* nested modules *)
	   sigEnv : env VarMap.map,         (* top-level, flat signature environment *)
	   outerEnv : env option            (* environment of the enclosing module *)
         }

    fun fromList ls = List.foldl VarMap.insert' VarMap.empty ls

    fun fresh (modRef, outerEnv) = ModEnv{
		         modRef=modRef, 
			 tyEnv=VarMap.empty, 
			 varEnv=VarMap.empty, 
			 modEnv=VarMap.empty, 
			 sigEnv=VarMap.empty, 
			 outerEnv=outerEnv
		       }
		
    fun varEnv (ModEnv{varEnv, ...}) = varEnv
    fun modRef (ModEnv{modRef, ...}) = modRef

    (* lookup a variable in the scope of the current module *)
    fun findInEnv (ModEnv (fields as {outerEnv, ...}), select, x) = (case VarMap.find(select fields, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findInEnv(env, select, x))
	 (* found a value *)
	 | SOME v => SOME v)	      

    fun findTy (env, tv) = findInEnv (env, #tyEnv, tv)
    fun findVar (env, v) = findInEnv (env, #varEnv, v)
    fun findMod (env, v) = findInEnv (env, #modEnv, v)
    fun findSig (env, v) = findInEnv (env, #sigEnv, v)

    fun insertTy (ModEnv {modRef, tyEnv, varEnv, modEnv, sigEnv, outerEnv}, tv, x) = 
	ModEnv{modRef=modRef, tyEnv=VarMap.insert (tyEnv, tv, x), varEnv=varEnv, modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertVar (ModEnv {modRef, varEnv, tyEnv, modEnv, sigEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=VarMap.insert (varEnv, v, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertMod (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=VarMap.insert (modEnv, v, x), sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertSig (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=modEnv, sigEnv=VarMap.insert (sigEnv, v, x), outerEnv=outerEnv}

    val inDomainTy = Option.isSome o findTy
    val inDomainVar = Option.isSome o findVar

  (* type realization environment *)
    structure RealizationEnv = TyCon.Map
    type realization_env = ty_def RealizationEnv.map

  (* maps for modules *)
    structure ModuleMap = BinaryMapFn (
                type ord_key = AST.module_ref
		fun compare (AST.MOD{id=id1, ...}, AST.MOD{id=id2, ...}) = Stamp.compare (id1, id2))

    type module_map = (env * env * AST.module) ModuleMap.map

  end (* ModuleEnv *)
