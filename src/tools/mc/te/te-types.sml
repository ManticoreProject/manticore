(* te-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for type-and-effect analysis.
 *)

structure TETypes =
  struct

    structure T = Types

    type effects = Effects.effects

    datatype ty_scheme = TyScheme of (T.tyvar list * ety)

    and ty
      = VarTy of T.tyvar * effects
      | ConTy of (ety list * T.tycon) * effects
      | FunTy of ety * effects * ety 
      | TupleTy of (ety list) * effects

    withtype ety = ty * effects

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
