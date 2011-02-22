(* translate-types-ft.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate AST types into FT interface types.
 * Note: this is not flattening of types, which takes place in another module.
 * This is just a straightforward translation.
 *
 * As the type is not flattened here, the translation translates the input 
 * into a representation type, and pairs the "copied" result with the original.
 * EX: int parr parr ---> < int parr parr / int parr parr >
 *   (even though after flattening the type will be
 *      < int parr parr / { int ; nd lf } >)
 *
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

structure TranslateTypesFT = struct

  structure T = Types
  structure F = FTTypes

  fun unsupported func descrip = let
    val msg = concat ["TranslateTypesFT: in ", func, " ", descrip, " unsupported."]
    in
      raise Fail msg
    end 

  fun ty_scheme _ = unsupported "ty_scheme" "type schemes"

  fun repr (t : T.ty) : F.repr_ty = 
    (case t
       of T.ErrorTy => unsupported "ty" "ErrorTy"
	| T.MetaTy _ => unsupported "ty" "MetaTy"
	| T.VarTy a => F.VarTy a
	| T.ConTy (ts, c) => F.ConTy (List.map repr ts, tycon c)
	| T.FunTy (dom, rng) => F.FunTy (repr dom, repr rng)
	| T.TupleTy ts => F.TupleTy (List.map repr ts)
      (* end case *))

  and tycon (c as T.Tyc {stamp, name, arity, params, props, def}) : F.tycon = 
       (case def
	  of T.AbsTyc => F.Tyc {stamp=stamp, name=name, arity=arity, 
				params=params, props=props, def=F.AbsTyc}
	   | T.DataTyc {nCons, cons} => let
               val dt = F.DataTyc {nCons=ref(!nCons), cons=ref []}
	       val c' = F.Tyc {stamp=stamp, name=name, arity=arity, 
			       params=params, props=props, def=dt}
	       fun lp ([], acc) = List.rev acc
		 | lp (c::cs, acc) = let
		     val T.DCon {id, name, owner, argTy} = c
		     val ic = F.DCon {id=id, name=name, owner=c', 
				      argTy=Option.map repr argTy}
		     in
		       lp (cs, ic::acc)
		     end
	       val cons' = lp (!cons, [])
	       val _ = setCons (dt, cons') (* backpatch new def *)
               in
		 c'
	       end
         (* end case *))

  and setCons (F.DataTyc {cons, ...}, ds) = (cons := ds)
    | setCons (F.AbsTyc, _) = raise Fail "this should be unreachable"

  fun ty (t : T.ty) : F.ty = F.IR (t, repr t)

  fun trTy (t : T.ty) : F.ty = let
    val t' = TypeUtil.prune t
    in
      ty t'
    end

  val trTycon = tycon

end
