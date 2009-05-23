(* binding-env.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Environment for the bound-variable check.
 *)

structure BindingEnv : sig

    type ty_bind = ProgramParseTree.PML2.ty_bind
    type var_bind = ProgramParseTree.PML2.var_bind
    type mod_bind = ProgramParseTree.PML2.mod_bind
    type sig_id = ProgramParseTree.PML2.sig_id

    type bom_var = ProgramParseTree.PML2.BOMParseTree.var_bind
    type bom_ty_def = ProgramParseTree.PML2.BOMParseTree.ty_def
    type bom_hlop = ProgramParseTree.PML2.BOMParseTree.hlop_bind

    type sig_env
    type mod_env

    type bom_env
    type env

    val empty : Atom.atom -> env
    val freshEnv : Atom.atom * env option -> env
    val freshEnv' : Atom.atom * env option * bom_env * mod_env * sig_env -> env
  (* get the outer environment *)
    val outerEnv    : env -> env option
    val nameOf      : env -> Atom.atom
    val sigEnv      : env -> sig_env
    val bomEnv      : env -> bom_env
    val modEnv      : env -> mod_env

  (* value bindings *)
    datatype val_bind
      = Con of var_bind
      | Var of var_bind

  (* type bindings *)
    datatype type_bind
      = AbsTyc of ty_bind       (* abstract type constructors *)
      | DataTyc of ty_bind      (* data type constructors *)
      | TypeExp of ty_bind      (* type expressions *)

    val tyId : type_bind -> ty_bind

  (* PML environment *)
    val insertDataTyc : env * Atom.atom * bom_ty_def -> env
    val insertAbsTyc  : env * Atom.atom * bom_ty_def -> env
    val insertTy      : env * Atom.atom * ty_bind -> env
    val insertType    : env * Atom.atom * type_bind -> env
    val insertDataCon : env * Atom.atom * var_bind * ty_bind -> env
    val insertVal     : env * Atom.atom * val_bind -> env
    val insertMod     : env * Atom.atom * (mod_bind * env) -> env
    val insertSig     : env * Atom.atom * (sig_id * env) -> env

    val findTy  : env * Atom.atom -> type_bind option
    val findVal : env * Atom.atom -> val_bind option
    val findMod : env * Atom.atom -> (mod_bind * env) option
    val findSig : env * Atom.atom -> (sig_id * env) option

  (* inline BOM environment *)
    val insertBOMVar  : env * Atom.atom * bom_var -> env
    val insertBOMTy   : env * Atom.atom * bom_ty_def -> env
    val insertBOMHLOp : env * Atom.atom * bom_hlop -> env

    val findBOMVar  : env * Atom.atom -> bom_var option
    val findBOMTy   : env * Atom.atom -> bom_ty_def option
    val findBOMHLOp : env * Atom.atom -> bom_hlop option

  (* get the data constructors associated with a datatype *)
    val getDataCons : ProgramParseTree.Var.var -> (Atom.atom * ProgramParseTree.Var.var) list

    val matchValsByName : env * env -> (Atom.atom * val_bind * val_bind) list
    val matchModsByName : env * env -> (Atom.atom * (mod_bind * env) * (mod_bind * env)) list
    val matchTysByName : env * env -> (Atom.atom * type_bind * type_bind) list

  (* filter out unwanted elements from an environment *)
    val filterEnv : env * (Atom.atom -> bool) -> env

  (* lookup the qualified id where the hlop is bound *)
    val getHLOpPath : bom_hlop -> string list

  (* C Functions *)
  (* Note that C functions have global scope *)
    val defineCFun    : (Atom.atom * bom_var) -> unit
    val findCFun      : Atom.atom -> bom_var option

  end = struct

    structure PT1 = ProgramParseTree.PML1
    structure PT2 = ProgramParseTree.PML2
    structure Var = ProgramParseTree.Var
    structure Map = AtomMap

    type ty_bind = PT2.ty_bind
    type var_bind = PT2.var_bind
    type mod_bind = PT2.mod_bind
    type sig_id = PT2.sig_id

    type bom_var = PT2.BOMParseTree.var_bind
    type bom_ty_def = PT2.BOMParseTree.ty_def
    type bom_hlop = PT2.BOMParseTree.hlop_bind

  (* environment for inline BOM *)
    datatype bom_env = BOMEnv of {
	  varEnv : bom_var Map.map,                 (* BOM functions *)
	  hlopEnv : bom_hlop Map.map,               (* HLOps *)
	  tyEnv : bom_ty_def Map.map                (* typedefs *)
        }

    val emptyBOMEnv = BOMEnv{
	    varEnv = Map.empty,
	    hlopEnv = Map.empty,
	    tyEnv = Map.empty
	  }

  (* value bindings *)
    datatype val_bind
      = Con of var_bind         (* data constructors *)
      | Var of var_bind         (* ordinary variables or overloaded variables *)

  (* type bindings *)
    datatype type_bind
      = AbsTyc of ty_bind       (* abstract type constructors *)
      | DataTyc of ty_bind      (* data type constructors *)
      | TypeExp of ty_bind      (* type expressions *)

    type ty_env = type_bind Map.map
    type var_env = val_bind Map.map
  (* map from qualified identifiers to flat variables (stamps) *)
    datatype env = Env of {
	    name     : Atom.atom,                      (* name of the module *)
	    tyEnv    : ty_env,                         (* type names *)
	    varEnv   : var_env,                        (* PML variables *)
	    bomEnv   : bom_env,                        (* inline BOM *)
	    modEnv   : (mod_bind * env) Map.map,       (* modules *)
	    sigEnv   : (sig_id * env) Map.map,         (* signatures *)
	    outerEnv : env option                      (* enclosing module *)
	  }

    type mod_env = (mod_bind * env) Map.map
    type sig_env = (sig_id * env) Map.map

    fun freshEnv' (name, outerEnv, bomEnv, modEnv, sigEnv) = Env {
	    name = name,
	    tyEnv = Map.empty,
	    varEnv = Map.empty,
	    bomEnv = bomEnv, 
	    modEnv = modEnv,
	    sigEnv = sigEnv,
	    outerEnv = outerEnv
	  }

    fun freshEnv (name, outerEnv) = freshEnv'(name, outerEnv, emptyBOMEnv, Map.empty, Map.empty)

    fun bomEnv (Env{bomEnv, ...}) = bomEnv
    fun sigEnv (Env{sigEnv, ...}) = sigEnv
    fun modEnv (Env{modEnv, ...}) = modEnv
    fun varEnv (Env{varEnv, ...}) = varEnv
    fun tyEnv (Env{tyEnv, ...}) = tyEnv
    fun nameOf (Env{name, ...}) = name
    fun outerEnv (Env{outerEnv, ...}) = outerEnv

    fun empty name = freshEnv (name, NONE)

    fun fromList ls = List.foldl Map.insert' Map.empty ls

    fun tyId (AbsTyc id) = id
      | tyId (DataTyc id) = id
      | tyId (TypeExp id) = id

  (* get the path owned by an environment *)
    fun pathOfEnv (Env{outerEnv=NONE, ...}, path) = path
      | pathOfEnv (Env{name, outerEnv=SOME env, ...}, path) = pathOfEnv(env, name :: path)
    val pathToAtom = Atom.atom o String.concatWith "." o List.map Atom.toString

  (* BOM environment operations *)
    local 
      fun insertVar (BOMEnv {varEnv, hlopEnv, tyEnv}, id, x) =
	    BOMEnv {varEnv=Map.insert(varEnv, id, x), hlopEnv=hlopEnv, tyEnv=tyEnv}
      fun insertHLOp (BOMEnv {varEnv, hlopEnv, tyEnv}, id, x) =
	    BOMEnv {hlopEnv=Map.insert(hlopEnv, id, x), varEnv=varEnv, tyEnv=tyEnv}
      fun insertTy (BOMEnv {varEnv, hlopEnv, tyEnv}, id, x) =
	    BOMEnv {hlopEnv=hlopEnv, varEnv=varEnv, tyEnv=Map.insert(tyEnv, id, x)}
    (* remember a HLOp's full path, e.g., Future1.@touch *)
      val {
	getFn=getHLOpPath : Var.var -> string list, 
	setFn=setHLOpPath : (Var.var * string list) -> unit, ...
      } = 
	  Var.newProp (fn _ => [])

    (* maps qualified ids to BOM types *)
      val tyPathMp : ty_bind AtomTable.hash_table = AtomTable.mkTable(128, Fail "Path table")
      fun addTyPath (env, id, x) = AtomTable.insert tyPathMp (pathToAtom(pathOfEnv(env, [id])), x)
    in
  (* insert an ordinary BOM variable *)
    fun insertBOMVar (Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	Env{name=name, tyEnv=tyEnv, varEnv=varEnv, bomEnv=insertVar(bomEnv, id, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv}
  (* insert a hlop *)
    fun insertBOMHLOp (env as Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = (
	(* retain the HLOp's full path *)
	setHLOpPath(x, List.map Atom.toString (pathOfEnv(env, [id])));
	Env{name=name, tyEnv=tyEnv, varEnv=varEnv, bomEnv=insertHLOp(bomEnv, id, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
  (* insert a BOM type *)
    fun insertBOMTy (env as Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = (
	(* retain the qualified id associated with the BOM type *)
	addTyPath(env, id, x);
	Env{name=name, tyEnv=tyEnv, varEnv=varEnv, bomEnv=insertTy(bomEnv, id, x), modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
  (* get the qualified identifier associated with a hlop *)
    val getHLOpPath = getHLOpPath
    end

    (* lookup a variable in the scope of the current module *)
    fun findInEnv (Env (fields as {outerEnv, ...}), select, x) = (case Map.find(select fields, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findInEnv(env, select, x))
	 (* found a value *)
	 | SOME v => SOME v)	      

    fun findTy (env, tv) = findInEnv (env, #tyEnv, tv)
    fun findVal (env, v) = findInEnv (env, #varEnv, v)
    fun findMod (env, v) = findInEnv (env, #modEnv, v)
    fun findSig (env, v) = findInEnv (env, #sigEnv, v)

    fun findBOMVar (Env{bomEnv=BOMEnv {varEnv, ...}, outerEnv, ...}, x) = (case Map.find(varEnv, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findBOMVar(env, x))
	 (* found a value *)
	 | SOME v => SOME v)

    fun findBOMHLOp (Env{bomEnv=BOMEnv {hlopEnv, ...}, outerEnv, ...}, x) = (case Map.find(hlopEnv, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findBOMHLOp(env, x))
	 (* found a value *)
	 | SOME v => SOME v)

    fun findBOMTy (Env{bomEnv=BOMEnv{tyEnv, ...}, outerEnv, ...}, x) = (case Map.find(tyEnv, x)
        of NONE => 
	   (* x is not bound in this module, so check the enclosing module *)
	   (case outerEnv
	     of NONE => NONE
	      | SOME env => findBOMTy(env, x))
	 (* found a value *)
	 | SOME v => SOME v)

  (* constrains env2 to contain only those keys that are also in env1 *)
    fun intersect (env1, env2) = Map.intersectWith (fn (x1, x2) => x2) (env1, env2)

    local
      (* map datatypes to their constructors *)
	val {
	  getFn=getDataCons : Var.var -> (Atom.atom * Var.var) list, 
	  setFn=setDataCons : (Var.var * (Atom.atom * Var.var) list) -> unit, ...
	} = 
	    Var.newProp (fn _ => [])
    in
  (* PML operations *)
    fun insertVal (env as Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = (
	  Env{name=name, tyEnv=tyEnv, varEnv=Map.insert(varEnv, id, x), bomEnv=bomEnv, modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
    fun insertType (env as Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = (
	  Env{name=name, tyEnv=Map.insert(tyEnv, id, x), varEnv=varEnv, bomEnv=bomEnv, modEnv=modEnv, sigEnv=sigEnv, outerEnv=outerEnv})
    fun insertMod (Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	  Env{name=name, tyEnv=tyEnv, varEnv=varEnv, bomEnv=bomEnv, modEnv=Map.insert(modEnv, id, x), sigEnv=sigEnv, outerEnv=outerEnv}
    fun insertSig (Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, id, x) = 
	  Env{name=name, tyEnv=tyEnv, varEnv=varEnv, bomEnv=bomEnv, modEnv=modEnv, sigEnv=Map.insert(sigEnv, id, x), outerEnv=outerEnv}
    fun insertTy (env, id, x) = insertType(env, id, TypeExp x)
    fun insertDataTyc (env, id, x : ty_bind) = let
	(* make datatypes visible to inline BOM programs *)
	  val env = insertBOMTy(env, id, x)
	(* as well as PML programs ... *)
	  in
	    insertType(env, id, DataTyc x)
	  end
    fun insertAbsTyc (env, id, x) = insertType(env, id, AbsTyc x)
    fun insertDataCon (env, id, x, dataTy) = let
	  val cons = getDataCons dataTy
	  in
	    setDataCons(dataTy, (id, x) :: cons);
	    insertVal(env, id, Con x)
	  end
  (* get the constructors for a given datatype *)
    val getDataCons = getDataCons
    end

  (* lists elements in the module environment that have a matching name in the constraining environment *)
    fun matchByName find (cEnv, mEnv) = let
	  fun match (id, cX, matches) = (
	        case find(mEnv, id)
		 of NONE => matches
		  | SOME m => (id, cX, m) :: matches
  	        (* end case *))
          in
	    Map.foldli match [] cEnv
	  end

    fun matchValsByName (cEnv as Env{varEnv, ...}, mEnv) = matchByName findVal (varEnv, mEnv)
    fun matchTysByName (cEnv as Env{tyEnv, ...}, mEnv) = matchByName findTy (tyEnv, mEnv)
    fun matchModsByName (cEnv as Env{modEnv, ...}, mEnv) = matchByName findMod (modEnv, mEnv)

    fun filterByKey filter = Map.filteri (filter o #1)

    fun filterBOMEnv (BOMEnv{varEnv, hlopEnv, tyEnv}, filter) =
	  BOMEnv{varEnv=filterByKey filter varEnv,
		 hlopEnv=filterByKey filter hlopEnv,
		 tyEnv=filterByKey filter tyEnv
		}

  (* filter out unwanted elements from an environment *)
    fun filterEnv (Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}, filter) = 
	  Env{name=name,
	      tyEnv=filterByKey filter tyEnv,
	      varEnv=filterByKey filter varEnv,
	      bomEnv=filterBOMEnv(bomEnv, filter),
	      modEnv=filterByKey filter modEnv,
	      sigEnv=sigEnv,
	      outerEnv=outerEnv
	     }

    fun varToString (id, (Var v | Con v)) = "\tvar "^Atom.toString id^" "^Var.toString v

    fun varEnvToString vEnv = String.concatWith "\n" (List.map  varToString (Map.listItemsi vEnv))

    fun toString (Env{name, tyEnv, varEnv, bomEnv, modEnv, sigEnv, outerEnv}) =
	String.concat [
	  Atom.toString name,
	  varEnvToString varEnv
	]

  (* C functions *)
  (* Note that C functions have global scope *)
    local 
	structure ATbl = AtomTable
	val tbl : Var.var ATbl.hash_table = AtomTable.mkTable (128, Fail "C function table")
    in
    val findCFun = ATbl.find tbl
    val defineCFun = ATbl.insert tbl
    end

  end
