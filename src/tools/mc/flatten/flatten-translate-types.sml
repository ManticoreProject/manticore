(* flatten-translate-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST types into FT types. "ID" translation.
 * 
 * NOTE: The names of these type transformations in this phase are confusing.
 *
 * FlattenTranslateTypes and FlattenTypes are different, as follows:
 *
 * - FlattenTranslateTypes is the straight translation of AST types into their
 *     equivalents in FTTypes, but the types are left basically in their 
 *     original form.
 *
 * - FlattenTypes is the actual flattening of types, where, say, a nested
 *     array type is transformed into its corresponding flat array type.
 *
 *)

structure FlattenTranslateTypes = struct

  structure ATy = Types (* AST types *)
  structure FTy = FTTypes
  structure FE  = FlattenEnv

  fun unsupported func descrip = let
    val msg = concat ["TranslateTypesFT: in ", func, " ", descrip, " unsupported."]
    in
      raise Fail msg
    end 

 fun copyTyc (newDef, c as ATy.Tyc {stamp, name, arity, params, props, def}) =
        FTy.Tyc {stamp = stamp,
		 name = name,
		 arity = arity,
		 params = params,
		 props = props (* does this need deep copying? FIXME *),
		 def = newDef,
		 interface = c}

  fun setCons (FTy.DataTyc {cons, ...}, ds) = (cons := ds)
    | setCons (FTy.AbsTyc, _) = raise Fail "setCons: unexpected AbsTyc"

  fun trTy (env : FE.env, t : ATy.ty) : FTy.ty = let
    fun unsup msg = unsupported "trTy" msg
    fun ty (ATy.ErrorTy) = unsup "ErrorTy"
      | ty (ATy.MetaTy _) = unsup "MetaTy" 
          (* FIXME I need to support MetaTy *)
          (* I don't understand why, but it's supported in translate *)
          (* (AST -> BOM) so it *must* be supported here too. -ams *)
      | ty (ATy.VarTy a) = FTy.VarTy a
      | ty (ATy.ConTy (ts, c)) = let
	  val ts' = List.map ty ts
          val c' = trTycon (env, c)
          in
	    FTy.ConTy (ts', c')
	  end
      | ty (ATy.FunTy (t, u)) = FTy.FunTy (ty t, ty u)
      | ty (t as ATy.TupleTy ts) = 
          (* note: we record the interface ty at tuple translation *)
          FTy.TupleTy (t, List.map ty ts)
    in
      ty t
    end

    and trTycon (env, c as ATy.Tyc {stamp, name, arity, params, props, def}) = 
         (case FE.findTyc (env, c)
	    of SOME fc => fc
	     | NONE =>
                (case def
		   of ATy.AbsTyc => copyTyc (FTy.AbsTyc, c)
		    | ATy.DataTyc {nCons, cons} => let
		        val def' = 
			  FTy.DataTyc {nCons = ref(!nCons),
				       cons = ref nil (* to be backpatched later...*)}
			val c' = copyTyc (def', c)
			val ds = !cons
		        val ds' = List.map (trDCon (env, c')) ds
	                in
			  setCons (def', ds');
			  FE.insertTyc (env, c, c');
			  c'
	                end
		     (* end case *))
	   (* end case *))

    and trDCon (env : FE.env, ownerTyc : FTy.tycon) 
	       (d as ATy.DCon {id, name, owner, argTy}) =
         (case FE.findDCon (env, d)
	    of SOME d' => d'
	     | NONE => let
                 val d' = FTy.DCon {id = id,
				    name = name,
				    owner = ownerTyc,
				    argTy = NONE, (* to be backpatched (type might be recursive)... *)
				    interface = d}
		 val _ = FE.insertCon (env, d, d')
		 val ty' = Option.map (fn t => trTy (env, t)) argTy
		 val d'' = setArgTy (d', ty')
	         in
		   d''
	         end
	   (* end case *))

    and setArgTy (d: FTy.dcon, optT : FTy.ty option) = let
          val (FTy.DCon {id, name, owner, argTy, interface}) = d
          in
	    FTy.DCon {id=id, name=name, owner=owner, interface=interface,
		      argTy=optT}
          end

  fun trScheme (env : FE.env, ATy.TyScheme (vs, t)) : FTy.ty_scheme = 
    FTy.TyScheme (vs, trTy (env, t))

end
