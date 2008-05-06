structure MatchSig :> sig

  (* check that the module's signature matches the given signature, and return the
   * final signature for sealing the module
   *)
    val match : {err : Error.err_stream, loc : Error.span, modEnv : Env.module_env, sigEnv : Env.module_env} -> Env.module_env

  (* takes a signature and some type revelations and returns the signature with those types revealed *)
    val reveal : (Env.module_env * Env.ty_env) -> Env.module_env

  end = struct

    structure Ty = Types

  (* FIXME: the following is a hack to avoid threading the error stream through
   * all of the typechecking code.  Eventually, we should fix this, since otherwise
   * it is a space leak.
   *)
    val errStrm = ref(Error.mkErrStream "<bogus>")

    fun error (span, msg) = Error.errorAt (!errStrm, span, "matching signatures: " :: msg)

    exception DeclNotFound
    exception NotFound 

    fun declOf NONE = raise DeclNotFound
      | declOf (SOME v) = v

  (* dcon substitution *)
    fun substDCon tbl (Ty.DCon {id, name, owner, argTy}) =
	Ty.DCon {id=id, name=name, owner=owner, argTy=Option.map (substTy tbl) argTy}

    and substTyCon tbl tyc = Option.getOpt (TyCon.Tbl.find tbl tyc, tyc)

  (* tycon substitution *)
    and substTy tbl ty = (case ty
        of Ty.MetaTy (Ty.MVar {stamp, info=ref (Ty.INSTANCE ty)}) =>
	   Ty.MetaTy (Ty.MVar {stamp=Stamp.new(), info=ref (Ty.INSTANCE (substTy tbl ty))})
	 | Ty.ConTy (tys, tyc) => (case TyCon.Tbl.find tbl tyc
           of NONE => Ty.ConTy(List.map (substTy tbl) tys, tyc)
	    | SOME tyc' => Ty.ConTy(List.map (substTy tbl) tys, tyc')
           (* end case *))
	 | Ty.FunTy (ty1, ty2) => Ty.FunTy(substTy tbl ty1, substTy tbl ty2)
	 | Ty.TupleTy tys => Ty.TupleTy (List.map (substTy tbl) tys)
	 | ty => ty
        (* end case *))

    fun substTyScheme tbl (Ty.TyScheme (tvs, ty)) = Ty.TyScheme(tvs, substTy tbl ty)

  (* takes a renaming table and a tycon and return the tycon's name, a fresh copy of the tycon, and
   * a finisher function. to complete the copy, client code must run the finisher function after
   * mutually recursive tycons have been copied.
   *)
    fun freshTyCon tbl (tyc as Ty.Tyc{stamp, name, arity, params, props, def}) = let
        val (def, finishFn) = (case def
				of Ty.AbsTyc => (Ty.AbsTyc, fn () => ())
				 | Ty.DataTyc {nCons, cons} => let
				   val cons' = ref []
				   fun finishFn () = cons := List.map (substDCon tbl) (!cons)
				   in
				      (Ty.DataTyc{nCons=nCons, cons=cons'}, finishFn)
                                   end
			      (* end case *))
	val tyc' = Ty.Tyc{stamp=Stamp.new(), name=name, arity=arity, params=params, props=props, def=def}
        in
	   TyCon.Tbl.insert tbl (tyc, tyc');
	   (tyc', finishFn)
        end

    fun copyTyCon tbl (tyc, env) = 
	(case TyCon.Tbl.find tbl tyc
	  of NONE => freshTyCon tbl tyc
	   | SOME tyc => (tyc, fn () => ())
	(* end case *))
	
    fun copyTyDef tbl (id, Env.TyDef tys, (env, finishFns)) = 
	(Env.insert(env, id, Env.TyDef (substTyScheme tbl tys)), finishFns)
      | copyTyDef tbl (id, Env.TyCon tyc, (env, finishFns)) = let
	val (tyc', finishFn) = copyTyCon tbl (tyc, env)
	in
          (Env.insert(env, id, Env.TyCon tyc'), finishFn :: finishFns)
        end

    fun copyVarDef tbl (id, vbind, env) = (case vbind
        of Env.Con (Ty.DCon {id=dcid, name, owner, argTy}) => let
	   val dcon = Ty.DCon{id=dcid, name=name, owner=substTyCon tbl owner, argTy=Option.map (substTy tbl) argTy}
           in
              Env.insert(env, id, Env.Con dcon)
           end
	 | Env.Var v => let
	   val ty = Var.typeOf v
	   in
	       (*Var.setType(v, ref (substTyScheme tbl ty));*)
	       Env.insert(env, id, Env.Var v)
	   end
	 | _ => raise Fail "impossible"
      (* end case *))

    fun copyMod tbl (Env.ModEnv{tyEnv=mTyEnv, varEnv=mVarEnv, modEnv=mModEnv, sigEnv, outerEnv, ...}) = let
	val (tyEnv', tyFinishFns) = Env.Map.foldli (copyTyDef tbl) (Env.empty, []) mTyEnv
	val varEnv' = Env.Map.foldli (copyVarDef tbl) Env.empty mVarEnv
	val modEnv' = Env.Map.foldli (fn (id, m, env) => Env.Map.insert(env, id, copyMod tbl m)) Env.empty mModEnv
        in
	  List.app (fn f => f()) (List.rev tyFinishFns);
	  Env.ModEnv{stamp=Stamp.new(), tyEnv=tyEnv', varEnv=varEnv', modEnv=modEnv', sigEnv=sigEnv, outerEnv=outerEnv}
        end

  (* takes environments for a module and its constraining signature and returns the residual signature for the 
   * module. this signature contains fresh variables for type constructors and variable definitions and 
   * data constructors.
   *)
    fun deepCopy (sigEnv, modEnv) = let
	val tbl = TyCon.Tbl.mkTable(128, NotFound)
        in
	   copyMod tbl modEnv
	end

  (* data cons must be sorted *)
    fun matchDCons loc (Ty.DCon {id=modId, name=modName, owner=modOwner, argTy=modArgTy},
			Ty.DCon {id=sigId, name=sigName, owner=sigOwner, argTy=sigArgTy}) =
	if not (Atom.same(modName, sigName))
           then error(loc, ["datacon missing name ", Atom.toString sigName])
        else (case (modArgTy, sigArgTy)
	       of (NONE, NONE) => ()
		| (SOME sigArgTy, SOME modArgTy) => 
		  if Unify.unifiable(sigArgTy, modArgTy)
		     then ()
		     else error(loc, ["data constructor argument", Atom.toString sigName])
             (* end case *))

    fun matchTypes loc (modTyEnv, sigTyEnv) = let
	val findIt = declOf o Env.find
	(* match each type definition in the signature to a corresponding type definition in the module *)
	fun match (id, Env.TyDef sigTs) = (case findIt(modTyEnv, id)
            of Env.TyDef modTs => 
	       (* match typedefs *)
	       if Unify.unifiableTS(modTs, sigTs)
	       then ()
	       else error (loc, [
			   "types for ", Atom.toString id, " are not equal\n",
			   "\tsignature: ", TypeUtil.schemeToString sigTs, "\n",
			   "\tmodule: ", TypeUtil.schemeToString modTs])
	     | Env.TyCon _ => error (loc, ["cannot match typedef with tycon for ", Atom.toString id])
           (* end case *))
	  | match (id, Env.TyCon sigTc) = (case (sigTc, findIt(modTyEnv, id))
            of (Ty.Tyc{arity=a1, def=Ty.AbsTyc, ...}, Env.TyDef (Ty.TyScheme (tvs, _))) => 
	     (* the type in the signature is abstract *)
	       if a1 <> List.length tvs
	          then error (loc, ["mismatched type arities for ", Atom.toString id])
                  else ()
	     | (Ty.Tyc{arity=a1, def=Ty.AbsTyc, ...}, Env.TyCon (Ty.Tyc{arity=a2, ...})) => 
	     (* the type in the signature is abstract *)
	       if a1 <> a2
	          then error (loc, ["mismatched type arities for ", Atom.toString id])
                  else ()
	     | (Ty.Tyc{arity=a1, def=Ty.DataTyc{nCons=ncs1, cons=cs1}, ...}, 
		Env.TyCon (Ty.Tyc{arity=a2, def=Ty.DataTyc{nCons=ncs2, cons=cs2}, ...})) =>
	       (* the type in the signature is a datatype *)
	       if a1 <> a2
                  then error (loc, ["mismatched type arities for ", Atom.toString id])
               else if !ncs1 <> !ncs2
                  then error (loc, ["mismatched number of data constructors for ", Atom.toString id])
               else ListPair.app (matchDCons loc) (!cs1, !cs2)
            (* end case *))

	fun doMatch (id, tydef) = match (id, tydef) 
	    handle DeclNotFound => error (loc, ["missing type declaration ", Atom.toString id])
	in
	    Env.Map.appi doMatch sigTyEnv
	end

    fun matchVars loc (modVarEnv, sigVarEnv) = let
	val findIt = declOf o Env.find
	fun match (id, Env.Var sigVar) = (case findIt(modVarEnv, id)
            of Env.Var modVar => 
	       if Unify.unifiableTS(Var.typeOf sigVar, Var.typeOf modVar)
	          then ()
	          else error(loc, ["cannot unify types for ", Atom.toString id])
	     | Env.Con dcon =>
	       if Unify.unifiableTS(Var.typeOf sigVar, DataCon.typeOf dcon)
	          then ()
	          else error(loc, ["cannot unify types for ", Atom.toString id])
	     | Env.Overload (ts, _) =>
	       if Unify.unifiableTS(Var.typeOf sigVar, ts)
	          then ()
	          else error(loc, ["cannot unify types for ", Atom.toString id])
	     | Env.EqOp _ => error(loc, ["invalid eq op"])
            (* end case *))
	  | match (id, Env.Con sigDCon) = (case findIt(modVarEnv, id)
            of Env.Con modDCon => matchDCons loc (sigDCon, modDCon)
	     | _ => error (loc, ["incompatible data con", Atom.toString id])
	    (* end case *))
	  | match (id, _) = error(loc, ["invalid value specification ", Atom.toString id])
	fun doMatch (id, vardef) = match(id, vardef)
	    handle DeclNotFound => error (loc, ["missing var declaration ", Atom.toString id])
	in
	   Env.Map.appi doMatch sigVarEnv
	end

  (* FIXME: handle error stream properly *)
    fun matchMods err loc (modEnv, sigEnv) = let
	val findIt = declOf o Env.find
	fun doMatch (id, sigEnv) = (match{err=err, loc=loc, modEnv=findIt(modEnv, id), sigEnv=sigEnv};
				    ())
	    handle DeclNotFound => error (loc, ["missing module declaration", Atom.toString id])
        in 
           Env.Map.appi doMatch sigEnv;
	   ()
        end

    and match {err, 
	       loc,
	       modEnv=modEnv as Env.ModEnv{tyEnv=modTyEnv, varEnv=modVarEnv, modEnv=modModEnv, ...}, 
	       sigEnv=sigEnv as Env.ModEnv{tyEnv=sigTyEnv, varEnv=sigVarEnv, modEnv=sigModEnv, ...}
	      } = let
	val _ = errStrm := err
        in
	   matchTypes loc (modTyEnv, sigTyEnv);
	   matchVars loc (modVarEnv, sigVarEnv);
	   matchMods err loc (modModEnv, sigModEnv);
	   deepCopy(sigEnv, modEnv)
        end

  (* FIXME *)
    fun reveal (env, revls) = env

  end (* MatchSig *)
