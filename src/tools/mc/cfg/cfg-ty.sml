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
      | T_Enum of Word.word	(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty		(* raw machine type *)
      | T_Wrap of raw_ty	(* boxed raw value *)
      | T_Tuple of ty list	(* heap-allocated tuple *)
      | T_OpenTuple of ty list	(* a tuple of unknown sizem, where we know the prefix. *)
    (* function/continuation types.  The type specifies the calling convention.  These
     * types should be used for labels and code addresses.
     *)
      | T_StdFun of {clos : ty, arg : ty, ret : ty, exh : ty}
      | T_StdCont of {clos : ty, arg : ty}
      | T_Code of ty list	(* includes both known functions and blocks *)

    val unitTy = T_Enum(0w0)
    val boolTy = T_Enum(0w1)	(* false = 0, true = 1 *)

    fun toString ty = let
	  fun tys2l ([], l) = l
	    | tys2l ([ty], l) = toString ty :: l
	    | tys2l (ty::tys, l) =
		toString ty ::
		  (List.foldr (fn (ty, l) => "," :: toString ty :: l) l tys)
	  in
	    case ty
	     of T_Any => "any"
	      | T_Enum w => concat["enum(0..", Word.fmt StringCvt.DEC w, ")"]
	      | T_Raw ty => RawTypes.toString ty
	      | T_Wrap ty => concat["wrap(", RawTypes.toString ty, ")"]
	      | T_Tuple tys => concat("(" :: tys2l(tys, [")"]))
	      | T_OpenTuple tys => concat("(" :: tys2l(tys, [",...)"]))
	      | T_StdFun{clos, arg, ret, exh} => concat("fun(" :: tys2l([clos, arg, ret, exh], [")"]))
	      | T_StdCont{clos, arg} => concat("cont(" :: tys2l([clos, arg], [")"]))
	      | T_Code tys => concat("code(" :: tys2l(tys, [")"]))
	    (* end case *)
	  end

    fun stdContTy (cpTy, argTy) = let
	  val cpTy = (case cpTy
		 of T_Any => T_StdCont{
			clos=T_OpenTuple[T_StdCont{clos=T_Any, arg=argTy}],
			arg=argTy
		      }
		  | T_StdCont _ => cpTy
		  | ty => raise Fail(concat["stdContTy(", toString cpTy, ", ", toString argTy, ")"])
		(* end case *))
	  in
	    cpTy
	  end

    fun selectTy (i, ty) = let
	  fun err () = raise Fail(concat["selectTy(", Int.toString i, ", ", toString ty, ")"])
	  fun sel (_, []) = err()
	    | sel (0, ty::r) = ty
	    | sel (i, _::r) = sel(i-1, r)
	  in
	    case ty
	     of T_Tuple tys => sel(i, tys)
	      | T_OpenTuple tys => sel(i, tys)
	      | _ => err()
	    (* end case *)
	  end

  end
