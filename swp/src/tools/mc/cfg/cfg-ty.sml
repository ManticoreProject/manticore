(* cfg-ty.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the first-order CPS representation.
 *)

structure CFGTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty

    datatype ty
      = T_Any				(* unknown type; uniform representation *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_Tuple of bool * ty list	(* heap-allocated tuple; the boolean is true for *)
					(* mutable tuples *)
      | T_OpenTuple of ty list		(* an immutable tuple of unknown size, where we know the prefix. *)
      | T_Addr of ty			(* address of a tuple's field *)
      | T_CFun of CFunctions.c_proto	(* a C function prototype *)
      | T_VProc				(* address of runtime vproc structure *)
    (* function/continuation types.  The type specifies the calling convention.  These
     * types should be used for labels and code addresses.
     *)
      | T_StdFun of {clos : ty, args : ty list, ret : ty, exh : ty}
      | T_StdCont of {clos : ty, args : ty list}
      | T_KnownFunc of {clos : ty, args : ty list}
      | T_Block of {args : ty list}

    val unitTy = T_Enum(0w0)
    val boolTy = T_Enum(0w1)	(* false = 0, true = 1 *)

  (* kinds for CFG types *)
    datatype kind
      = K_RAW		(* raw bits *)
      | K_BOXED		(* heap pointer *)
      | K_UNBOXED	(* tagged integer *)
      | K_UNIFORM	(* either K_BOXED or K_UNBOXED *)
      | K_ANY           (* pointer sized, can be cast to/from any *)
      | K_TYPE		(* type (any of the above kinds) *)

  end
