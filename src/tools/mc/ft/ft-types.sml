(* ft-types.sml
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

structure FTTypes = struct

  datatype interface_ty
    = I_ConTy of (interface_ty list * interface_tycon)
    | I_FunTy of interface_ty * interface_ty
    | I_TupleTy of interface_ty list
    (* TySchemes, TyVars omitted for the time being *)

  and interface_tycon 
    = I_Tyc of {
        stamp : Stamp.stamp,
	name : Atom.atom,
	arity : int,
	params : interface_ty list, (* NOTE no tyvars *)
	props : PropList.holder,
	def : interface_tycon_def}

  and interface_tycon_def 
    = I_AbsTyc
    | I_DataTyc of {
        nCons : int ref,
	cons  : interface_dcon list ref
      }

  and interface_dcon      
    = I_DCon of {
        id : int,
	name : Atom.atom,
	owner : interface_tycon,
	argTy : interface_ty option
      }

  datatype nt_ty
    = Lf
    | Nd of nt_ty

  datatype repr_ty
    = R_ConTy of (repr_ty list * repr_tycon)
    | R_FunTy of repr_ty * repr_ty
    | R_TupleTy of repr_ty list
    | R_FlatArrayTy of repr_ty * nt_ty

  and repr_tycon 
    = R_Tyc of {
        stamp : Stamp.stamp,
	name : Atom.atom,
	arity : int,
	params : repr_ty list, (* NOTE no tyvars *)
	props : PropList.holder,
	def : repr_tycon_def
      }

  and repr_tycon_def 
    = R_AbsTyc
    | R_DataTyc of {
        nCons : int ref,
	cons : repr_dcon list ref
      }

  and repr_dcon
    = R_DCon of {
        id : int,
	name : Atom.atom,
	owner : repr_tycon,
	argTy : repr_ty option
      }

  datatype ty
    = I  of interface_ty
    | IR of interface_ty * repr_ty

end
