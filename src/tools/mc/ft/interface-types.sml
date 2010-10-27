(* interface-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)

(* The technical point here is that the notes/amsft paper
 * contains no account of type constructors (apart from perhaps
 * nullaries, in its non-specific reference to ground types).
 * So one needs to decide what to do with types like int option.
 *)

structure InterfaceTypes = struct

  datatype ty
    = ConTy of (ty list * tycon)
    | FunTy of ty * ty
    | TupleTy of ty list
    (* TySchemes, TyVars omitted for the time being *)

  and tycon 
    = Tyc of {
        stamp : Stamp.stamp,
	name : Atom.atom,
	arity : int,
	params : ty list, (* NOTE no tyvars *)
	props : PropList.holder,
	def : tycon_def}

  and tycon_def 
    = AbsTyc
    | DataTyc of {
        nCons : int ref,
	cons  : dcon list ref
      }

  and dcon      
    = DCon of {
        id : int,
	name : Atom.atom,
	owner : tycon,
	argTy : ty option
      }

end
