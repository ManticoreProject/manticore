(* bom-ty-pt.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse-tree representation of BOM types.
 *)
 
structure BOMTyPT =
   struct
   
    datatype raw_ty = datatype BOMTy.raw_ty

    datatype ty
      = T_Any				(* unknown type; uniform representation *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_Tuple of bool * ty list	(* heap-allocated tuple *)
      | T_Addr of ty
      | T_Fun of (ty list * ty list * ty list)
					(* function type; the second argument is the type of *)
					(* the exception continuation(s) *)
      | T_Cont of ty list		(* first-class continuation *)
      | T_CFun of CFunctions.c_proto	(* C functions *)
      | T_VProc				(* address of VProc runtime structure *)
      | T_TyCon of Atom.atom		(* high-level type constructor *)

  end
