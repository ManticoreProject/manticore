(* translate-types-ft.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST types into FT interface types.
 * Note: this is not flattening of types, which takes place in another module.
 * This is just a straightforward translation.
 * 
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure TranslateTypesFT = struct

  structure T = Types
  structure I = InterfaceTypes

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

  and tycon (c as T.Tyc {stamp, name, arity, params, props, def}) : I.tycon = 
       (case def
	  of T.AbsTyc => I.Tyc {stamp=stamp, name=name, arity=arity, 
				params=params, props=props, def=I.AbsTyc}
	   | T.DataTyc {nCons, cons} => let
               val dt = I.DataTyc {nCons=ref(!nCons), cons=ref []}
	       val c' = I.Tyc {stamp=stamp, name=name, arity=arity, 
			       params=params, props=props, def=dt}
	       fun lp ([], acc) = List.rev acc
		 | lp (c::cs, acc) = let
		     val T.DCon {id, name, owner, argTy} = c
		     val ic = I.DCon {id=id, name=name, owner=c', 
				      argTy=Option.map ty argTy}
		     in
		       lp (cs, ic::acc)
		     end
	       val cons' = lp (!cons, [])
	       val _ = setCons (dt, cons') (* backpatch new def *)
               in
		 c'
	       end
         (* end case *))

  and setCons (I.DataTyc {cons, ...}, ds) = (cons := ds)
    | setCons (I.AbsTyc, _) = raise Fail "this should be unreachable"

  fun translate (t : T.ty) : FTTypes.ty = let
    val p = (fn s => (print s; print "\n"))
(*
    val _ = p ("t: " ^ TypeUtil.toString t)
    val _ = p ("prune t: " ^ TypeUtil.toString (TypeUtil.prune t))
*)
    in
      FTTypes.I (ty (TypeUtil.prune t))
    end

end
