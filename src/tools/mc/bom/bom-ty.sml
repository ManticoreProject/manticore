(* bom-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty
    datatype tyc = datatype BOMTyCon.tyc
	
    datatype ty
      = T_Any			(* unknown type; uniform representation *)
      | T_Enum of Word.word	(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty		(* raw machine type *)
      | T_Wrap of raw_ty	(* boxed raw value *)
      | T_Tuple of ty list	(* heap-allocated tuple *)
      | T_Fun of (ty list * ty list * ty list)
				(* function type; the second argument is the type of *)
				(* the exception continuation(s) *)
      | T_Cont of ty list	(* first-class continuation *)
      | T_TyCon of tyc		(* high-level type constructor *)

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

(* T_Fun => fun( [...], ty1, ty2) *)
	      | T_Fun (tyl, ty1, ty2) => concat("fun( [" :: tys2l(tyl, ["]"]) :: toString ty1 :: "," :: toString ty2 :: ")")
	      | T_Cont tys => concat("cont(" :: tys2l(tys, [")"]))
	      | T_Tyc tyc => BOMTyCon.toString tyc
	    (* end case *)
	  end

  (* view a type as a function type *)
    fun asFunTy (T_Fun arg) = arg
      | asFunTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* get the return type(s) of a function type *)
    fun returnTy (T_Fun(_, _, ty)) = ty
      | returnTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* view as tycon *)
    fun asTyc (T_TyCon tyc) = tyc
      | asTyc ty = raise Fail("expected tyc, but found " ^ toString ty)

  end
