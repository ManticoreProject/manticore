(* c-functions.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CFunctions =
  struct

    datatype c_type
      = PointerTy
      | BaseTy of RawTypes.raw_ty
      | VoidTy				(* function return types only *)

    datatype c_fun = CFun of {
	name : Atom.atom,		(* name of the Manticore variable bound to *)
					(* this function *)
	externName : string,		(* external name *)
	retTy : c_type,			(* return type *)
	argTys : c_type list		(* argument type *)
      }

  end
