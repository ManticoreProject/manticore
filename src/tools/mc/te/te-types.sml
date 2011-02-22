(* te-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for type-and-effect analysis.
 *)

structure TETypes = struct

  structure T = Types

  type effects = Effects.effects

  datatype ty_scheme = TyScheme of (T.tyvar list * ety)

  and ty
    = VarTy of T.tyvar * effects
    | ConTy of (ety list * T.tycon) * effects
    | FunTy of (ety * effects * ety) * effects
    | TupleTy of (ety list) * effects
		 
  withtype ety = ty * effects

(* FIXME
  I need to think about what it is that the type and effect analysis produces.
  Do I decorate the whole AST with effects all over, so later I can just
  read them off? I think that is what I want, but if so I may have to think
  a little harder about what the EAST looks like.
*)

  fun effectsOf (t : ty) : effects = 
   (case t
      of VarTy (_, f) => f
       | ConTy (_, f) => f
       | FunTy (_, f) => f
       | TupleTy (_, f) => f
     (* end case *))

end

(*    = ErrorTy *)
(*    | MetaTy of meta *)

(*
    and meta = MVar of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    info : meta_info ref
	  }
    and meta_info
      = UNIV of int
      | CLASS of ty_class
      | INSTANCE of ty
*)
