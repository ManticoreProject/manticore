(* te-types.sml
 *
 * COPYRIGHT (c) 2011 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for type-and-effect analysis.
 *)

structure TETypes =
  struct

    type effects = Effects.effects

    datatype ty_scheme = TyScheme of (tyvar list * ty)

    and ty
      = VarTy of tyvar * effects
      | ConTy of (fty list * tycon) * effects
      | FunTy of fty * effects * fty 
      | TupleTy of (fty list) * effects

    and tyvar = TVar of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the varable name *)
	    class : ty_class option     (* optional type class *)
	  }

    and ty_class = Int | Float | Num | Order | Eq

    and tycon = Tyc of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the type name *)
	    arity : int,		(* number of type parameters *)
	    params : tyvar list,	(* type parameters *)
	    props : PropList.holder,
	    def : tycon_def		(* definition of tyc *)
	  }

    and tycon_def
      = AbsTyc
      | DataTyc of {
	    nCons : int ref,		(* number of constructors *)
	    cons : dcon list ref	(* list of data constructors *)
	  }

    and dcon = DCon of {
	    id : int,			(* this constructor's index in the cons list *)
	    name : Atom.atom,		(* the name of the constructor *)
	    owner : tycon,		(* the datatype for which this is a constructor *)
	    argTy : fty option		(* argument type; NONE for nullary constructors *)
	  }

    withtype fty = ty * effects

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
