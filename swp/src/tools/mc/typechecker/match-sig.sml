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
    val match : {loc : Error.span, modEnv : ModuleEnv.env, sigEnv : ModuleEnv.env} 
		    -> ModuleEnv.env

  (* takes a signature and some type revelations and returns the signature with those types revealed *)
    val reveal : (ModuleEnv.env * ModuleEnv.ty_env) -> ModuleEnv.env

  end = struct

    structure Ty = Types
    structure Env = ModuleEnv
    structure BVar = ProgramParseTree.Var

    val error = ErrorStream.error

    val varToString = ProgramParseTree.Var.nameOf

  (* takes an entry kind, constraining environment, and a module environment, and returns the list of
   * matching elements. if elements are missing, we report errors.
   *)
    fun matchAndReport (loc, entryKind, cEnv, mEnv) = let
          fun reportMissing ((id, c, NONE), xs) = (
	         error(loc, ["missing ", entryKind, " ", Atom.toString id]);
		 xs)
	    | reportMissing ((id, c, SOME m), xs) = 
	         (id, c, m) :: xs
          in
	     List.foldl reportMissing [] (Env.matchByName(cEnv, mEnv))
          end

  (* data cons must be sorted *)
    fun matchDCons loc 
		   (Ty.DCon {id=modId, name=modName, owner=modOwner, argTy=modArgTy},
		    Ty.DCon {id=sigId, name=sigName, owner=sigOwner, argTy=sigArgTy}) =
	if not (Atom.same(modName, sigName))
           then error(loc, ["datacon missing name ", Atom.toString sigName])
        else (case (modArgTy, sigArgTy)
	       of (NONE, NONE) => ()
		| (SOME sigArgTy, SOME modArgTy) => 
		  if MatchTy.matchTys(sigArgTy, modArgTy)
		     then ()
		     else error(loc, ["data constructor argument ", Atom.toString sigName])
             (* end case *))

    fun matchTypes loc (modTyEnv, sigTyEnv) = let
	fun match (id, Env.TyDef sigTs, Env.TyDef modTs) =
	       if MatchTy.match(sigTs, modTs)
	       then ()
	       else error (loc, [
			   "types for ", Atom.toString id, " are not equal\n",
			   "\tsignature: ", TypeUtil.schemeToString sigTs, "\n",
			   "\tmodule: ", TypeUtil.schemeToString modTs])
	  | match (id, Env.TyDef sigTs, Env.TyCon _) = 
	       error (loc, ["cannot match typedef with tycon for ", Atom.toString id])
	  | match (id, Env.TyCon (Ty.Tyc{arity=a1, def=Ty.AbsTyc, ...}), Env.TyDef (Ty.TyScheme (tvs, _))) = 
	       if a1 <> List.length tvs
	          then error (loc, ["mismatched type arities for ", Atom.toString id])
                  else ()
	  | match (id, Env.TyCon (Ty.Tyc{arity=a1, def=Ty.AbsTyc, ...}), Env.TyCon (Ty.Tyc{arity=a2, ...})) = 
	       if a1 <> a2
	          then error (loc, ["mismatched type arities for ", Atom.toString id])
                  else ()
	  | match (id, 
		   Env.TyCon (Ty.Tyc{arity=a1, def=Ty.DataTyc{nCons=ncs1, cons=cs1}, ...}), 
		   Env.TyCon (Ty.Tyc{arity=a2, def=Ty.DataTyc{nCons=ncs2, cons=cs2}, ...})) =
	      if a1 <> a2
                 then error (loc, ["mismatched type arities for ", Atom.toString id])
              else if !ncs1 <> !ncs2
                 then error (loc, ["mismatched number of data constructors for ", Atom.toString id])
              else ListPair.app (matchDCons loc) (!cs1, !cs2)
	in
	   List.app match (matchAndReport(loc, "type", sigTyEnv, modTyEnv))
	end

    fun matchVars loc (modVarEnv, sigVarEnv) = let
	fun match (id, Env.Var sigVar, Env.Var modVar) = let
	       val sigTyS = Var.typeOf sigVar
	       val modTyS = Var.typeOf modVar
	       in
	           if MatchTy.match(sigTyS, modTyS)
	              then ()
	              else error(loc, ["failed to match value specification ", Atom.toString id, "\n",
				       "signature: ", TypeUtil.schemeToString sigTyS, "\n",
				       "structure: ", TypeUtil.schemeToString modTyS
				])
               end
	  | match (id, Env.Var sigVar, Env.Con dcon) =
	       if MatchTy.match(Var.typeOf sigVar, DataCon.typeOf dcon)
	          then ()
	          else error(loc, ["cannot unify types for ", Atom.toString id])
	  | match (id, Env.Var sigVar, Env.Overload (ts, _)) =
	       if MatchTy.match(Var.typeOf sigVar, ts)
	          then ()
	          else error(loc, ["cannot unify types for ", Atom.toString id])
	  | match (id, Env.Var sigVar, Env.EqOp _) = 
	       error(loc, ["invalid eq op"])
	  | match (id, Env.Con sigDCon, Env.Con modDCon) =
	       matchDCons loc (sigDCon, modDCon)
	  | match (id, Env.Con sigDCon, _) = error (loc, ["incompatible data con", Atom.toString id])
	  | match (id, _, _) = error(loc, ["invalid value specification ", Atom.toString id])	
	in
	   List.app match (matchAndReport(loc, "value declaration", sigVarEnv, modVarEnv))
	end

    fun matchMods loc (modEnv , sigEnv) = let
	fun f (id, modEnv, sigEnv) = (
	    match{loc=loc, modEnv=modEnv, sigEnv=sigEnv}; 
	    ())
        in 
	   List.app f (matchAndReport(loc, "module", sigEnv, modEnv))
        end

    and match {loc,
	       modEnv=modEnv as Env.ModEnv{tyEnv=modTyEnv, varEnv=modVarEnv, modEnv=modModEnv, ...}, 
	       sigEnv=sigEnv as Env.ModEnv{tyEnv=sigTyEnv, varEnv=sigVarEnv, modEnv=sigModEnv, ...}
	      } = (
         (* set the realizations of type constructors *)
	   List.app Env.setRealizationOfTyc (Env.tyConDefs (sigTyEnv, modTyEnv));
         (* match specs in the constraining signature with declarations the implementing structure *)
	   matchTypes loc (modTyEnv, sigTyEnv);
	   matchVars loc (modVarEnv, sigVarEnv);
	   matchMods loc (modModEnv, sigModEnv);
         (* the fresh signature for the sealed module *)
	   CopySig.copy(sigEnv, modEnv))

  (* FIXME *)
    fun reveal (env, revls) = env

  end (* MatchSig *)
