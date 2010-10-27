(* representation-types.sml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the flattening transformation.
 * Supporting documents in 
 * /path/to/manti-papers/papers/notes/amsft
 *)
structure RepresentationTypes = struct

  datatype ty
    = ConTy of (ty list * tycon)
    | FunTy of ty * ty
    | TupleTy of ty list
    | FlatArrayTy of ty * NestingTreeTypes.ty

  and tycon 
    = Tyc of {
        stamp : Stamp.stamp,
	name : Atom.atom,
	arity : int,
	params : ty list, (* NOTE no tyvars *)
	props : PropList.holder,
	def : tycon_def
      }

  and tycon_def 
    = AbsTyc
    | DataTyc of {
        nCons : int ref,
	cons : dcon list ref
      }

  and dcon
    = DCon of {
        id : int,
	name : Atom.atom,
	owner : tycon,
	argTy : ty option
      }

end
