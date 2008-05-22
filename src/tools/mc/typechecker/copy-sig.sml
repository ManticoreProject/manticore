structure CopySig =
  struct

    structure Ty = Types
    structure Env = ModuleEnv

    exception NotFound 

    fun substDCon (Ty.DCon {id, name, owner, argTy}) =
	Ty.DCon {id=id, name=name, owner=owner, argTy=Option.map substTy argTy}

    and copyTyCon tyc = (case Env.getRealizationOfTyc tyc
          of SOME (Env.TyCon tyc') => tyc'
	   | _ => tyc
          (* end case *))
	
    and substTy ty = (case ty
        of Ty.MetaTy (Ty.MVar {stamp, info=ref (Ty.INSTANCE ty)}) =>
	   Ty.MetaTy (Ty.MVar {stamp=stamp, info=ref (Ty.INSTANCE (substTy ty))})
	 | Ty.ConTy (tys, tyc) => Ty.ConTy(List.map substTy tys, copyTyCon tyc)
	 | Ty.FunTy (ty1, ty2) => Ty.FunTy(substTy ty1, substTy ty2)
	 | Ty.TupleTy tys => Ty.TupleTy (List.map substTy tys)
	 | ty => ty
        (* end case *))

    fun substTyScheme (Ty.TyScheme (tvs, ty)) = Ty.TyScheme(tvs, substTy ty)

    fun copyTyDef (id, Env.TyDef tys, env) = 
	Env.VarMap.insert(env, id, Env.TyDef (substTyScheme tys))
      | copyTyDef (id, Env.TyCon tyc, env) = let
	val tyc' = copyTyCon tyc
	in
          Env.VarMap.insert(env, id, Env.TyCon tyc')
        end

    fun copyVarDef (id, vbind, env) = (case vbind
        of Env.Con (Ty.DCon {id=dcid, name, owner, argTy}) => let
	   val dcon = Ty.DCon{id=dcid, name=name, owner=copyTyCon owner, argTy=Option.map substTy argTy}
           in
              Env.VarMap.insert(env, id, Env.Con dcon)
           end
	 | Env.Var v => let
	   val ty' = substTyScheme (Var.typeOf v)
	   val v' = Var.newPoly(Var.nameOf v, ty')
	   in
	       Env.VarMap.insert(env, id, Env.Var v')
	   end
	 | _ => raise Fail "impossible"
      (* end case *))

    fun copyMod (Env.ModEnv{modRef=modRef as AST.MOD{name, formals, ...}, 
				tyEnv=mTyEnv, varEnv=mVarEnv, modEnv=mModEnv, 
				sigEnv, outerEnv, ...}) = let
	val tyEnv' = Env.VarMap.foldli copyTyDef Env.VarMap.empty mTyEnv
	val varEnv' = Env.VarMap.foldli copyVarDef Env.VarMap.empty mVarEnv
	val modEnv' = Env.VarMap.foldli (fn (id, m, env) => Env.VarMap.insert(env, id, copyMod m)) Env.VarMap.empty mModEnv
	val AST.MOD{name, formals, ...} = modRef
	val modRef = AST.MOD{name=name, id=Stamp.new(), formals=formals}
        in
	  Env.ModEnv{modRef=modRef, tyEnv=tyEnv', varEnv=varEnv', modEnv=modEnv', sigEnv=sigEnv, outerEnv=outerEnv}
        end

  (* given a type constructor from the constraining signature, create a fresh type constructor for the
   * sealing signature. note that datatype constructors may contain stale references.
   *)
    fun freshTyc (sigTyc as Ty.Tyc{name, arity, def=Ty.AbsTyc, ...}) = let
	   val tyc' = TyCon.newAbsTyc(TyCon.nameOf sigTyc, TyCon.arityOf sigTyc, false)
           in
	       Env.setRealizationOfTyc(tyc', Option.valOf(Env.getRealizationOfTyc sigTyc));
	       tyc'
	   end
      | freshTyc sigTyc = let
	    val SOME(Env.TyCon(modTyc as Ty.Tyc{params, def=Ty.DataTyc{cons, nCons}, ...})) = 
		       Env.getRealizationOfTyc sigTyc
	    val tyc' = TyCon.newDataTyc(TyCon.nameOf modTyc, params)
	    fun copyDCon (Ty.DCon {id, name, owner, argTy}) = 
		   Ty.DCon{id=id, name=name, owner=tyc', argTy=argTy}
	    val cons' = List.map copyDCon (!cons)
            in
	       Env.setRealizationOfTyc(modTyc, Env.TyCon tyc');
	       cons := cons';
	       tyc'
	    end

  (* create a type environment with fresh entries for type constructors *)
    fun freshTyEnv tyEnv = let
	  fun ins (id, Env.TyCon tyc, tyEnv) = Env.VarMap.insert(tyEnv, id, Env.TyCon (freshTyc tyc))
	    | ins (id, tyDef, tyEnv) = Env.VarMap.insert(tyEnv, id, tyDef)
          in
	     Env.VarMap.foldli ins Env.VarMap.empty tyEnv
          end 

  (* takes environments for a module and its constraining signature and returns the residual signature for the 
   * module. this signature contains fresh variables for type constructors and variable definitions and 
   * data constructors.
   *)
    fun copy (sigEnv, modEnv) = let

	val Env.ModEnv{tyEnv=sigTyEnv, varEnv=sigVarEnv, modEnv=sigModEnv, ...} = sigEnv
	val Env.ModEnv{modRef, tyEnv, varEnv, modEnv, outerEnv, sigEnv} = modEnv

	val tyEnv' = freshTyEnv sigTyEnv
	val modEnv' = Env.ModEnv{modRef=modRef, tyEnv=tyEnv', varEnv=sigVarEnv, modEnv=sigModEnv, sigEnv=sigEnv, outerEnv=outerEnv}

        in
	   copyMod modEnv'
	end

  end
