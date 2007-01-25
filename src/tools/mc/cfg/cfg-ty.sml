(* cfg-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the first-order CPS representation.
 *)

structure CFGTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty

    datatype ty
      = T_Any			(* unknown type; uniform representation *)
      | T_Bool			(* booleans *)
      | T_Raw of raw_ty		(* raw machine type *)
      | T_Wrap of raw_ty	(* boxed raw value *)
      | T_Tuple of ty list	(* heap-allocated tuple *)
    (* function/continuation types.  The type specifies the calling convention *)
      | T_StdFun of {clos : ty, arg : ty, ret : ty, exh : ty}
      | T_StdCont of {clos : ty, arg : ty}
      | T_Code of ty list	(* includes both known functions and blocks *)

    fun toString ty = let
	  fun tys2l ([], l) = l
	    | tys2l ([ty], l) = toString ty :: l
	    | tys2l (ty::tys, l) =
		toString ty ::
		  (List.foldr (fn (ty, l) => "," :: toString ty :: l) l tys)
	  in
	    case ty
	     of T_Any => "any"
	      | T_Bool => "bool"
	      | T_Raw ty => RawTypes.toString ty
	      | T_Wrap ty => concat["wrap(", RawTypes.toString ty, ")"]
	      | T_Tuple tys => concat("(" :: tys2l(tys, [")"]))
	      | T_StdFun{clos, arg, ret, exh} => concat("fun(" :: tys2l([clos, arg, ret, exh], [")"]))
	      | T_StdCont{clos, arg} => concat("cont(" :: tys2l([clos, arg], [")"]))
	      | T_Code tys => concat("code(" :: tys2l(tys, [")"]))
	    (* end case *)
	  end

  end
