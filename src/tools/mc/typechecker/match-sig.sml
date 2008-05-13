(* match-sig.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Signature matching.
 *)


structure MatchSig :> sig

  (* check that the module's signature matches the given signature, and return the
   * final signature for sealing the module
   *)
    val match : {err : Error.err_stream, loc : Error.span, modEnv : Env.module_env, sigEnv : Env.module_env} -> Env.module_env

  (* takes a signature and some type revelations and returns the signature with those types revealed *)
    val reveal : (Env.module_env * Env.ty_env) -> Env.module_env

  (* get the concrete definition of an abstract type *)
    val realizationOfTyc : Types.tycon -> Env.ty_def option

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
	   Ty.MetaTy (Ty.MVar {stamp=stamp, info=ref (Ty.INSTANCE (substTy tbl ty))})
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
	   val ty' = substTyScheme tbl (Var.typeOf v)
	   val v' = Var.newPoly(Var.nameOf v, ty')
	   in
	       Env.insert(env, id, Env.Var v')
	   end
	 | _ => raise Fail "impossible"
      (* end case *))

    fun copyMod tbl (Env.ModEnv{modRef=modRef as AST.MOD{name, formals, ...}, 
				tyEnv=mTyEnv, varEnv=mVarEnv, modEnv=mModEnv, 
				sigEnv, outerEnv, ...}) = let
	val (tyEnv', tyFinishFns) = Env.Map.foldli (copyTyDef tbl) (Env.empty, []) mTyEnv
	val varEnv' = Env.Map.foldli (copyVarDef tbl) Env.empty mVarEnv
	val modEnv' = Env.Map.foldli (fn (id, m, env) => Env.Map.insert(env, id, copyMod tbl m)) Env.empty mModEnv
	val AST.MOD{name, formals, ...} = modRef
	val modRef = AST.MOD{name=name, id=Stamp.new(), formals=formals}
        in
	  List.app (fn f => f()) (List.rev tyFinishFns);
	  Env.ModEnv{modRef=modRef, tyEnv=tyEnv', varEnv=varEnv', modEnv=modEnv', sigEnv=sigEnv, outerEnv=outerEnv}
        end

  (* get the concrete definition of an abstract type *)
    val {getFn=realizationOfTyc : Types.tycon -> Env.ty_def option, setFn, ...} = TyCon.newProp(fn _ => NONE)

  (* takes environments for a module and its constraining signature and returns the residual signature for the 
   * module. this signature contains fresh variables for type constructors and variable definitions and 
   * data constructors.
   *)
    fun deepCopy (sigEnv, modEnv) = let
	val tbl = TyCon.Tbl.mkTable(128, NotFound)

	(* if the signature contains abstract type definitions, replace their manifest
	 * counterparts in the module.
	 *)
	val Env.ModEnv{tyEnv=sigTyEnv, varEnv=sigVarEnv, modEnv=sigModEnv, ...} = sigEnv
	val Env.ModEnv{modRef, tyEnv, varEnv, modEnv, outerEnv, sigEnv} = modEnv
	fun abstractTyDef (id, Env.TyCon (tyc as Ty.Tyc{name, arity, def=Ty.AbsTyc, ...}), tyEnv) = 
	    (case Env.find(tyEnv, id)
              of NONE => tyEnv
	       | SOME tyd => let
		     val tyc' = TyCon.newAbsTyc(TyCon.nameOf tyc, TyCon.arityOf tyc, false)
		     in
		        setFn(tyc', SOME tyd);
		        TyCon.Tbl.insert tbl (tyc, tyc');
		        Env.insert(#1(Env.Map.remove(tyEnv, id)), id, Env.TyCon tyc')
		     end
            (* end case *))
	  | abstractTyDef (_, _, tyEnv) = tyEnv
	val tyEnv' = Env.Map.foldli abstractTyDef tyEnv sigTyEnv
	val modEnv' = Env.ModEnv{modRef=modRef, tyEnv=tyEnv', varEnv=sigVarEnv, modEnv=sigModEnv, sigEnv=sigEnv, outerEnv=outerEnv}

        in
	   copyMod tbl modEnv'
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

    fun matchTypes loc realizationEnv (modTyEnv, sigTyEnv) = let
	val findIt = declOf o Env.find
	(* match each type definition in the signature to a corresponding type definition in the module *)
	fun match (id, Env.TyDef sigTs) = (case findIt(modTyEnv, id)
            of Env.TyDef modTs => 
	       (* match typedefs *)
	       if MatchTy.match(realizationEnv, modTs, sigTs)
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

    fun matchVars loc realizationEnv (modVarEnv, sigVarEnv) = let
	val findIt = declOf o Env.find
	fun match (id, Env.Var sigVar) = (case findIt(modVarEnv, id)
            of Env.Var modVar => let
	       val sigTyS = Var.typeOf sigVar
	       val modTyS = Var.typeOf modVar
	       in
	           if MatchTy.match(realizationEnv, sigTyS, modTyS)
	              then ()
	              else error(loc, ["failed to match value specification ", Atom.toString id, "\n",
				       "signature: ", TypeUtil.schemeToString sigTyS, "\n",
				       "structure: ", TypeUtil.schemeToString modTyS
				])
               end
	     | Env.Con dcon =>
	       if MatchTy.match(realizationEnv, Var.typeOf sigVar, DataCon.typeOf dcon)
	          then ()
	          else error(loc, ["cannot unify types for ", Atom.toString id])
	     | Env.Overload (ts, _) =>
	       if MatchTy.match(realizationEnv, Var.typeOf sigVar, ts)
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

  (* FIXME *)
    fun matchMods err loc realizationEnv (modEnv, sigEnv) = let
	val findIt = declOf o Env.find
	fun doMatch (id, sigEnv) = (match{err=err, loc=loc, modEnv=findIt(modEnv, id), sigEnv=sigEnv};
raise Fail "";
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

	(* takes a type identifier and the corresponding abstract type in the signature and 
	 * adds to the realization environment the realization of the type in the module.
	 *)
	fun addRealization (id, Env.TyCon (tyc as Ty.Tyc {def=Ty.AbsTyc, ...}), realizationEnv) = 
	    (case Env.find(modTyEnv, id)
	      of SOME tyd => TyCon.Map.insert(realizationEnv, tyc, tyd)
	       | NONE => realizationEnv
	    (* end case *))
	  | addRealization (_, _, realizationEnv) = realizationEnv
	val realizationEnv = Env.Map.foldli addRealization TyCon.Map.empty sigTyEnv

        in
	   matchTypes loc realizationEnv (modTyEnv, sigTyEnv);
	   matchVars loc realizationEnv (modVarEnv, sigVarEnv);
	   matchMods err loc realizationEnv (modModEnv, sigModEnv);
	   deepCopy(sigEnv, modEnv)
        end

  (* FIXME *)
    fun reveal (env, revls) = env

  end (* MatchSig *)
