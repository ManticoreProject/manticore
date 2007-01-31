(* cps-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPSTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty

    datatype ty
      = T_Any			(* unknown type; uniform representation *)
      | T_Bool			(* booleans *)
      | T_Raw of raw_ty		(* raw machine type *)
      | T_Wrap of raw_ty	(* boxed raw value *)
      | T_Tuple of ty list	(* heap-allocated tuple *)
      | T_Fun of ty list
      | T_Cont of ty list

    fun toString ty = let
	  fun r2s T_Byte = "byte"
	    | r2s T_Short = "short"
	    | r2s T_Int = "int"
	    | r2s T_Long = "long"
	    | r2s T_Float = "float"
	    | r2s T_Double = "double"
	    | r2s T_Vec128 = "vec128"
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
	      | T_Fun tys => concat("fun(" :: tys2l(tys, [")"]))
	      | T_Cont tys => concat("cont(" :: tys2l(tys, [")"]))
	    (* end case *)
	  end

  end
