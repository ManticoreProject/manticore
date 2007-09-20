(* types.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Types =
  struct

    datatype ty_scheme = TyScheme of (tyvar list * ty)

    and ty
      = ErrorTy
      | MetaTy of meta
      | ClassTy of class
      | VarTy of tyvar
      | ConTy of (ty list * tycon)
      | FunTy of ty * ty
      | TupleTy of ty list

    and meta = MVar of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    info : meta_info ref
	  }

    and meta_info
      = UNIV of int
      | INSTANCE of ty

    and class = Class of class_info ref

    and class_info
      = CLASS of ty_class
      | RESOLVED of ty

    and ty_class
      = Int
      | Float
      | Num
      | Order
      | Eq

    and tyvar = TVar of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the varable name *)
	    class : ty_class option     (* optional type class *)
	  }

    and tycon
      = AbsTyc of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the type name *)
	    arity : int,		(* number of type parameters *)
	    eq : bool			(* is it an equality type? *)
	  }
      | DataTyc of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the type name *)
	    params : tyvar list,	(* type parameters *)
	    nCons : int ref,		(* number of constructors *)
	    cons : dcon list ref	(* list of data constructors *)
	  }

    and dcon = DCon of {
	    id : int,			(* this constructor's index in the cons list *)
	    name : Atom.atom,		(* the name of the constructor *)
	    owner : tycon,		(* the datatype for which this is a constructor *)
	    argTy : ty option		(* argument type; NONE for nullary constructors *)
	  }

  end
