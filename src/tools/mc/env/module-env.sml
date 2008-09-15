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

    datatype ty_def 
      = TyDef of Types.ty_scheme                                   (* PML type definition *)
      | TyCon of Types.tycon                                       (* PML type constructor *)
      | BOMTyDef of ProgramParseTree.PML2.BOMParseTree.ty          (* inline BOM type definition *)

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

    fun find (ModEnv fields, select, x) = VarMap.find(select fields, x)
    fun findInEnvs ([], select, x) = NONE
      | findInEnvs (env :: envs, select, x) = (
	case find(env, select, x)
         of NONE => findInEnvs (envs, select, x)
	  | SOME v => SOME v
	(* end case *))

  (* look in all environments that might be in lexical scope of env (NOTE: this list is larger than
   * actual lexical scope.)
   *)
    fun envsInScope (env as ModEnv{outerEnv, ...}) = let
	fun f (env as ModEnv{modEnv, ...}) = let
	    val envs' = VarMap.listItems modEnv
	    in
	      env :: List.concat(List.map f envs') @ envs'
	    end
	in
	   case outerEnv
	    of NONE => f env
	     | SOME outerEnv => f outerEnv @ f env
	end

  (* lookup a variable in the scope of the current module *)
    fun findInEnv (env, select, x) = 
	(case find(env, select, x)
          of NONE => findInEnvs(envsInScope env, select, x)
	   | SOME v => SOME v
	(* end case *))

  (* lookup a variable in several modules *)

    fun findTy (env, tv) = findInEnv (env, #tyEnv, tv)
    fun findVar (env, v) = findInEnv (env, #varEnv, v)
    fun findMod (env, v) = findInEnv (env, #modEnv, v)
    fun findSig (env, v) = findInEnv (env, #sigEnv, v)

  (* realization of abstract types *)
    local
        val {getFn : Types.tycon -> ty_def option, setFn, ...} = 
                           TyCon.newProp(fn _ => NONE)
    in
      fun setRealizationOfTyc (tyc, tyd) = setFn(tyc, SOME tyd)
      val getRealizationOfTyc = getFn
    end

  (* map parse-tree value definitions to ast value definitions *)
    val {
           getFn=getValBind : ProgramParseTree.Var.var -> val_bind option, 
	   setFn=setValBind : (ProgramParseTree.Var.var * val_bind option) -> unit, ...
        } = 
	   ProgramParseTree.Var.newProp (fn _ => NONE)
  (* map parse-tree type definitions to ast type definitions *)
    val {
           getFn=getTyDef : ProgramParseTree.Var.var -> ty_def option, 
	   setFn=setTyDef : (ProgramParseTree.Var.var * ty_def option) -> unit, ...
        } = 
	   ProgramParseTree.Var.newProp (fn _ => NONE)

  (* map parse-tree type definitions to ast type definitions *)
    val {
           getFn=getPrimTyDef : ProgramParseTree.Var.var -> ProgramParseTree.PML2.BOMParseTree.ty option, 
	   setFn=setPrimTyDef : (ProgramParseTree.Var.var * ProgramParseTree.PML2.BOMParseTree.ty option) -> unit, ...
        } = 
	   ProgramParseTree.Var.newProp (fn _ => NONE)

    fun bindVal (v, x) = setValBind (v, SOME x)

    fun insertTy (ModEnv {modRef, tyEnv, varEnv, modEnv, sigEnv, outerEnv}, tv, x) = (
	setTyDef(tv, SOME x);
	ModEnv{modRef=modRef, tyEnv=VarMap.insert (tyEnv, tv, x), varEnv=varEnv, modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
  (* primtive types
   *   type tyvars tv = _prim(bty)
   * where tv is a type binding and bty is a primitive BOM type. we bind tv in both BOM and Manticore. the
   * Manticore type is an abstract type.
   *)
    fun insertPrimTy (env, tv, tyc, bty) = (
	setRealizationOfTyc(tyc, BOMTyDef bty);
	setPrimTyDef(tv, SOME bty);
	insertTy(env, tv, TyCon tyc))
    fun insertVar (ModEnv {modRef, varEnv, tyEnv, modEnv, sigEnv, outerEnv}, v, x) = (
	setValBind(v, SOME x);
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=VarMap.insert (varEnv, v, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
    fun insertMod (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=VarMap.insert (modEnv, v, x), sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertSig (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=modEnv, sigEnv=VarMap.insert (sigEnv, v, x), outerEnv=outerEnv}

    val inDomainTy = Option.isSome o findTy
    val inDomainVar = Option.isSome o findVar

    structure ModuleMap = BinaryMapFn (
                type ord_key = AST.module_ref
		fun compare (AST.MOD{id=id1, ...}, AST.MOD{id=id2, ...}) = Stamp.compare (id1, id2))

    type module_map = (env * env * AST.module) ModuleMap.map

  (* takes an environment where the keys are parse-tree variables and returns an environment
   * where the keys are atoms.
   *)
    fun cvtEnv env = let
	   val cvtVar = Atom.atom o ProgramParseTree.Var.nameOf
	   fun ins (v, x, env) = AtomMap.insert(env, cvtVar v, x)
           in
	      VarMap.foldli ins AtomMap.empty env
	   end

  (* match elements in the environments that have the same names *)
    fun matchByName (cEnv, mEnv) = let
	   fun find (id, cX, matches) = (case AtomMap.find(cvtEnv mEnv, id)
                  of NONE => (id, cX, NONE) :: matches
		   | SOME mX => (id, cX, SOME mX) :: matches
                  (* end case *))
	   in
	      AtomMap.foldli find [] (cvtEnv cEnv)
   	   end

  (* take signature and module type environments and return the type constructors in the signature
   * paired with their definitions in the module, e.g.,
   * 
   *   tyConDefs(sig type t end, struct type t = int end)
   *          ==>
   *   [(t, AbsTyc, Env.TyDef int)]
   *)
    fun tyConDefs (sigTyEnv, modTyEnv) = let
  	   fun f ((_, TyCon tyc, SOME tyDef), ms) = (tyc, tyDef) :: ms
	     | f (_, ms) = ms
           in
	      List.foldl f [] (matchByName(sigTyEnv, modTyEnv))
	   end

    local
	val {getFn : Var.var -> Var.var option, setFn, ...} =
	              Var.newProp(fn _ => NONE)
    in
      fun setVar (v, v') = setFn(v, SOME v')
      val getVar = getFn
    end

    fun tyDefToString (id, TyDef ts) = ProgramParseTree.Var.toString id^" = "^TypeUtil.schemeToString ts
      | tyDefToString (id, TyCon tc) = ProgramParseTree.Var.toString id^" = "^TyCon.toString tc

    fun valBindToString (id, Con dc) = ProgramParseTree.Var.toString id^" = "^DataCon.toString dc
      | valBindToString (id, Var v)  = ProgramParseTree.Var.toString id^" = "^Var.toString v^" : "^TypeUtil.schemeToString (Var.typeOf v)
      | valBindToString (id, Overload (ts, vs)) = ProgramParseTree.Var.toString id^" = "^"..."
      | valBindToString (id, EqOp v) = ProgramParseTree.Var.toString id^" = "^Var.toString v

    fun toString (ModEnv {modRef=AST.MOD{name, ...}, varEnv, tyEnv, modEnv, ...}) = let
	   val tys = String.concatWith "\n" (List.map tyDefToString (VarMap.listItemsi tyEnv))
	   val vars = String.concatWith "\n" (List.map valBindToString (VarMap.listItemsi varEnv))
	   val mods = String.concatWith "\n" (List.map (toString o #2) (VarMap.listItemsi modEnv))
	   in
	      String.concatWith "\n" [
	         "sig "^Atom.toString name^" = ", 
		 tys,
		 vars,
		 mods
	      ]
	   end

    fun typeOfValBind (Con dc) = DataCon.typeOf dc
      | typeOfValBind (Var v) = Var.typeOf v
      | typeOfValBind (Overload (ts, _)) = ts
      | typeOfValBind (EqOp v) = Var.typeOf v

  end (* ModuleEnv *)
