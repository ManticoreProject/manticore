(* bom-ty.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty

  (* kinds for BOM types *)
    datatype kind
      = K_RAW		(* raw bits *)
      | K_BOXED		(* heap pointer *)
      | K_UNBOXED	(* tagged integer *)
      | K_UNIFORM	(* either K_BOXED or K_UNBOXED *)
      | K_TYPE		(* type (any of the above kinds) *)

  (* type variables are used for HLOps that have not been instantiated yet; regular BOM
   * code is monomorphic.
   *)
    datatype ty_var = TV of Stamp.stamp

    datatype ty
      = T_Param of ty_var
      | T_Raw of raw_ty
      | T_Con of tyc * ty list		(* NOTE: for monomorphic code, the ty list is empty *)
      | T_Tuple of ty list
      | T_Record of (int * bool * ty) list
      | T_Fun of ty list * ty list * ty list
      | T_Cont of ty list
      | T_Array of ty
      | T_Vector of ty
      | T_Addr of ty
      | T_Bignum
      | T_VProc
      | T_Any
      | T_CFun of CFunctions.c_proto	(* C functions *)
(* do we want this?
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
*)

(* QUESTION: should we fold Array, Vector, Addr, Bignum, etc into tyc as AbsTyc? *)

    and tyc			      (* high-level type constructor *)
      = DataTyc of {
	  name : string,
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  nNullary : int,		(* the number of nullary constructors *)
	  cons : data_con list ref,	(* list of constructors *)
	  props : PropList.holder
	}
(*
      | AbsTyc of {
	  name : string,
	  stamp : Stamp.stamp,
	  arity : int
	}
*)

    and data_con = DCon of {	      (* a data-constructor function *)
	  name : string,		(* the name of the constructor *)
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  argTy : ty list,		(* type(s) of argument(s) to this constructor *)
	  myTyc : tyc,			(* the datatype that this constructor belongs to *)
	  props : PropList.holder
	}

    val unitTy = T_Tuple[]

  (* construct an immutable tuple type; nullary tuples are unit type and singleton
   * tuples have a direct representation.
   *)
    fun tupleTy [] = unitTy
      | tupleTy [ty] = ty
      | tupleTy tys = T_Tuple tys

  end
