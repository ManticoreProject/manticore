(* ft-module-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Environment representation for modules.
 *)

structure FTModuleEnv =
  struct

    structure T = FTTypes

    structure VarMap = ProgramParseTree.Var.Map
    structure TyVarMap = AtomMap

    datatype ty_def 
      = TyDef of T.ty_scheme                    (* PML type definition *)
      | TyCon of T.tycon                        (* PML type constructor *)
      | BOMTyDef of ProgramParseTree.PML2.BOMParseTree.ty (* inline BOM type definition *)

  (* value identifiers may be data constructors, variables, or overloaded variables. *)
    datatype val_bind
      = Con of T.dcon
      | Var of FLAST.var
      | Overload of T.ty_scheme * FLAST.var list
      | EqOp of FLAST.var

    type ty_env = ty_def VarMap.map		(* TE in the semantics *)
    type tyvar_env = Types.tyvar AtomMap.map	(* TVE in the semantics *)
    type var_env = val_bind VarMap.map		(* VE in the semantics *)

  (* module environment that mirrors the actual nesting structure of a module *)
    datatype env
      = ModEnv of {
	   modRef : FLAST.module_ref,       (* module identification *)
	   tyEnv : ty_env,                  (* type definitions *)
	   varEnv : var_env,                (* variable definitions *)
	   modEnv : env VarMap.map,         (* nested modules *)
	   sigEnv : env VarMap.map,         (* top-level, flat signature environment *)
	   outerEnv : env option            (* environment of the enclosing module *)
         }

    val empty = ModEnv{
	    modRef = FLAST.MOD{
		id=Stamp.new(),
		name=Atom.atom "PrimBasis",
		formals=NONE,
		expansionOpts=ref []
	      }, 
	    modEnv = VarMap.empty, 
	    sigEnv = VarMap.empty, 
	    outerEnv = NONE,
	    tyEnv = VarMap.empty, 
	    varEnv = VarMap.empty
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
      | findInEnvs (env :: envs, select, x) = (case find(env, select, x)
	   of NONE => findInEnvs (envs, select, x)
	    | SOME v => SOME v
	  (* end case *))

  (* look in all environments that might be in lexical scope of env (NOTE: this list is larger than
   * actual lexical scope.
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
        val {getFn : T.tycon -> ty_def option, setFn, ...} = 
                           FTTyCon.newProp(fn _ => NONE)
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

  (* map parse-tree BOM type definitions to ast type definitions *)
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
   * where tv is a type binding and bty is a primitive BOM type. we bind tv in both BOM and PML.
   * The PML type is an abstract type.
   *)
    fun insertPrimTy (env, tv, tyc, bty) = (
	  setRealizationOfTyc(tyc, BOMTyDef bty);
	  setPrimTyDef(tv, SOME bty);
	  insertTy(env, tv, TyCon tyc))
    fun insertVar (ModEnv{modRef, varEnv, tyEnv, modEnv, sigEnv, outerEnv}, v, x) = (
	setValBind(v, SOME x);
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=VarMap.insert (varEnv, v, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
    fun insertMod (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=VarMap.insert (modEnv, v, x), sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertSig (ModEnv {modRef, modEnv, sigEnv, tyEnv, varEnv, outerEnv}, v, x) = 
	ModEnv{modRef=modRef, tyEnv=tyEnv, varEnv=varEnv, modEnv=modEnv, sigEnv=VarMap.insert (sigEnv, v, x), outerEnv=outerEnv}

    val inDomainTy = Option.isSome o findTy
    val inDomainVar = Option.isSome o findVar

    structure ModuleMap = BinaryMapFn (
      type ord_key = FLAST.module_ref
      fun compare (FLAST.MOD{id=id1, ...}, FLAST.MOD{id=id2, ...}) = Stamp.compare (id1, id2)
      )

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

    fun typeOfValBind (Con dc) = FTDataCon.typeOf dc
      | typeOfValBind (Var v) = FTVar.typeOf v
      | typeOfValBind (Overload (ts, _)) = ts
      | typeOfValBind (EqOp v) = FTVar.typeOf v

(* mkPrim : ModuleEnv.env -> env *)
(* This is a special-case copying function for creating primFTEnv for InitialBasis. *)
(* It translates the elements from primEnv one by one. *)
(* Three of primEnv's components --- modEnv, sigEnv, and outerEnv --- are empty, *)
(* so we can skip copying those (although we assert that they are empty here). *)
  local 
    structure ME = ModuleEnv
    fun isEmpty (m : VarMap.map) : bool = (VarMap.numItems m = 0)
    fun assert msg fact = raise Fail ("assert: " ^ msg)
    fun mk (mr, te, ve) = ModEnv {modRef = mr, tyEnv = te,
				  varEnv = ve, modEnv = VarMap.empty,
				  tyEnv = VarMap.empty, outerEnv = NONE}
    fun copyModRef (AST.MOD {name, id, formals, expansionOpts}) =
	  FLAST.MOD {name=name,
		     id=id,
		     formals=Option.map (List.map copyModRef) formals,
		     expansionOpts = ref(!expansionOpts)}
    fun copyTyEnv (te : ME.ty_def VarMap.map) : ty_def VarMap.map = let
          fun lp ([], acc) = acc
	    | lp ((x,d)::t, acc) = let
                val d' = copyTyDef d
                in
                  lp (t, VarMap.insert (acc, x, d')) 
                end
          in
            lp (VarMap.listItemsi te, VarMap.empty)
          end          
  in
    fun mkPrim (m : ME.env) : env = let
      val ME.MedEnv {modRef, tyEnv, varEnv, modEnv, sigEnv, outerEnv} = m
      (* assert that modEnv and sigEnv are empty, and outerEnv is NONE *)
      val _ = assert "modEnv not empty" (isEmpty modEnv)
      val _ = assert "sigEnv not empty" (isEmpty sigEnv)
      val _ = assert "outerEnv is SOME _" ((not o isSome) outerEnv)
      val modRef' = copyModRef modRef
      val tyEnv' = copyTyEnv tyEnv
      val varEnv' = copyVarEnv varEnv
      in
        mk (modRef', tyEnv', varEnv')
      end (* mkPrim *)
    end (* local *)  

    fun fromModuleEnv (m : ModuleEnv.env) : env = let
          val prim = PrimFlattenEnv.primFlattenEnv
	  val ModuleEnv.ModEnv {modRef, tyEnv, varEnv, modEnv, sigEnv, outerEnv} = m
	  val modRef' = copyModRef modRef
	  val tyEnv' = copyTyEnv (prim, tyEnv)
	  val varEnv' = copyVarEnv (prim, varEnv)
	  val modEnv' = copyModEnv modEnv
	  val sigEnv' = copySigEnv sigEnv
	  val outerEnv' = copyOuterEnv outerEnv
          in
	    ModEnv {modRef = modRef',
		    tyEnv = tyEnv',
		    varEnv = varEnv',
		    modEnv = modEnv',
		    sigEnv = sigEnv',
		    outerEnv = outerEnv'}
          end
    and copyTyEnv _ = raise Fail "todo"
    and copyVarEnv _ = raise Fail "todo"
    and copyModEnv (m : ModuleEnv.env ProgramParseTree.Var.Map.map) = let
(* FIXME respect the nesting structure here... *)
	  val emp = ProgramParseTree.Var.Map.empty
          val kvs = ProgramParseTree.Var.Map.listItemsi m
          fun lp ([], acc) = acc
	    | lp ((k,v)::t, acc) = let
                val v' =  fromModuleEnv v
                in
		  lp (t, ProgramParseTree.Var.Map.insert (acc, k, v'))
	        end
          in
	    lp (kvs, emp)
          end
    and copySigEnv s = copyModEnv s (* same structure *)
    and copyOuterEnv NONE = NONE
      | copyOuterEnv (SOME e) = SOME (fromModuleEnv e)

(* stringification *)
    fun tyDefToString (id, TyDef ts) = ProgramParseTree.Var.toString id ^
				       " = " ^
				       FTTypeUtil.schemeToString ts
      | tyDefToString (id, TyCon tc) = ProgramParseTree.Var.toString id ^
				       " = " ^
				       FTTyCon.toString tc
      | tyDefToString (id, BOMTyDef _) = raise Fail "todo"

    fun valBindToString (id, Con dc) = ProgramParseTree.Var.toString id ^
				       " = " ^
				       FTDataCon.toString dc
      | valBindToString (id, Var v)  = 
          String.concat [ProgramParseTree.Var.toString id," = ", 
			 FTVar.toString v, " : ", 
			 FTTypeUtil.schemeToString (FTVar.typeOf v)]
      | valBindToString (id, Overload (ts, vs)) = ProgramParseTree.Var.toString id ^
						  " = " ^ 
						  "[[Overload]]"
      | valBindToString (id, EqOp v) = ProgramParseTree.Var.toString id ^
				       " = " ^
				       FTVar.toString v

    fun toString (ModEnv {modRef=FLAST.MOD{name, ...}, varEnv, tyEnv, modEnv, ...}) = let
	   val tys = String.concatWith "\n" (List.map tyDefToString (VarMap.listItemsi tyEnv))
	   val vars = String.concatWith "\n" (List.map valBindToString (VarMap.listItemsi varEnv))
	   val mods = String.concatWith "\n" (List.map (toString o #2) (VarMap.listItemsi modEnv))
	   in
	     String.concat [
		 "sig ", Atom.toString name, " =\n", 
		  tys, "\n",
		  vars, "\n",
		  mods
	       ]
	   end

  end (* FTModuleEnv *)
