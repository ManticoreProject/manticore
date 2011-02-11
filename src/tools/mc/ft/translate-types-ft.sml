(* translate-types-ft.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST types into FT interface types.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure TranslateTypesFT = struct

  structure T = Types
  structure I = InterfaceTypes
  structure N = NestingTreeTypes
  structure R = RepresentationTypes

  fun unsupported func descrip = let
    val msg = concat ["TranslateTypesFT: in ", func, " ", descrip, " unsupported."]
    in
      raise Fail msg
    end 

  fun ty_scheme _ = unsupported "ty_scheme" "type schemes"

  fun ty (t : T.ty) : I.ty = 
    (case t
       of T.ErrorTy => unsupported "ty" "ErrorTy"
	| T.MetaTy _ => unsupported "ty" "MetaTy"
	| T.VarTy a => I.VarTy a
	| T.ConTy (ts, c) => I.ConTy (List.map ty ts, tycon c)
	| T.FunTy (t, u) => I.FunTy (ty t, ty u)
	| T.TupleTy ts => I.TupleTy (List.map ty ts))

  and tycon (c as T.Tyc {stamp, name, arity, params, props, def}) : I.tycon = let
        val def' = copyDef def
        in
          I.Tyc {stamp=stamp, name=name, arity=arity, 
		 params=params, props=props, def=def'}
        end

  and copyDef (T.AbsTyc) = I.AbsTyc
    | copyDef (T.DataTyc {nCons, cons}) = let
        val nConsCopy = ref (!nCons)
	val consCopy = deepCopy cons
        in
          I.DataTyc {nCons=nConsCopy, cons=deepCopy cons}
        end

  and deepCopy (cons: T.dcon list ref) = let
    val cs = !cons
    val cs' = List.map dcon cs
    in
      ref cs'
    end

  and dcon (T.DCon {id, name, owner, argTy}) =
    I.DCon {id=id, 
	    name=name, 
	    owner=tycon owner, 
	    argTy=Option.map ty argTy}

  fun translate (t : T.ty) : FTTypes.ty = let
    val p = (fn s => (print s; print "\n"))
    val _ = p ("t: " ^ TypeUtil.toString t)
    val _ = p ("prune t: " ^ TypeUtil.toString (TypeUtil.prune t))
    in
      FTTypes.I (ty (TypeUtil.prune t))
    end

end
