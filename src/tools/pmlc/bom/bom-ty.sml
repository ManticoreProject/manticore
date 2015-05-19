(* bom-ty.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTy : sig

    type ty_var = BOMTyVar.t

    type tyc = BOMTyc.t

    datatype raw_ty (* from common/raw-types.sml *)
      = Int8
      | UInt8
      | Int16
      | UInt16
      | Int32
      | UInt32
      | Int64
      | UInt64
      | Float32
      | Float64
      | Vec128		(* SSE *)
      | Vec256		(* AVX *)
      | Vec512		(* AVX-512 (Xeon Phi) *)

    datatype t
      = T_Param of ty_var			(* type parameter (only inside HLOps!) *)
      | T_Raw of raw_ty				(* machine types *)
      | T_Con of tyc * t list			(* type constructor *)
      | T_Record of (bool * t) list		(* record type; bool marks mutable fields *)
      | T_Packed of (int * bool * t) list	(* packed record *)
      | T_Fun of t list * t list * t list	(* function type (args/conts) -> results *)
      | T_Cont of t list			(* continuation *)
      | T_CFun of CFunctions.c_proto		(* C functions *)
      | T_Any					(* any type; an escape hatch that we may not need *)

  (* are two type equal? *)
    val equal : (t * t) -> bool

  (* is a cast from srcTy to dstTy valid? *)
    val validCast : {srcTy : t, dstTy : t} -> bool

  (* return a string representation of a type *)
    val toString : t -> string

  (* view a type as a function type *)
    val asFunTy : t -> (t list * t list * t list)

  (* get the return type(s) of a function type *)
    val returnTy : t -> t list

  (* view as tycon *)
    val asTyc : t -> BOMTyc.t

  (* construct an immutable tuple type; nullary tuples are unit type and singleton
   * tuples have a direct representation.
   *)
    val tupleTy : t list -> t

  (* select i'th type component from a tuple type *)
    val select : t * int -> t

  (* convert a C type into a BOM type with the same representation *)
    val ctypeToBOM : CFunctions.c_type -> t list

  (* some standard type constructors/types *)
    val arrayTy : t -> t
    val vectorTy : t -> t
    val addrTy : t -> t
    val vprocTy : t
    val unitTy : t

  end = struct

    type ty_var = BOMTyVar.t
    type tyc = BOMTyc.t
    datatype raw_ty = datatype RawTypes.raw_ty
    datatype t = datatype BOMRep.ty

  (* are two type equal? *)
    fun equal (ty1, ty2) = (case (ty1, ty2)
          of (T_Param tv1, T_Param tv2) => BOMTyVar.same(tv1, tv2)
	   | (T_Raw rt1, T_Raw rt2) => rt1 = rt2
	   | (T_Con(tyc1, tys1), T_Con(tyc2, tys2)) =>
		BOMTyc.same (tyc1, tyc2) andalso ListPair.allEq equal (tys1, tys2)
	   | (T_Record flds1, T_Record flds2) =>
		ListPair.allEq
		  (fn ((m1, ty1), (m2, ty2)) => (m1 = m2) andalso equal(ty1, ty2))
		    (flds1, flds2)
	   | (T_Packed flds1, T_Packed flds2) =>
		ListPair.allEq
		  (fn ((loc1, m1, ty1), (loc2, m2, ty2)) => (loc1 = loc2) andalso (m1 = m2) andalso equal(ty1, ty2))
		    (flds1, flds2)
	   | (T_Fun (argTys1, exhTys1, retTys1), 
	      T_Fun (argTys2, exhTys2, retTys2)) =>
		ListPair.allEq equal (argTys1, argTys2) andalso
		ListPair.allEq equal (exhTys1, exhTys2) andalso
		ListPair.allEq equal (retTys1, retTys2)
	   | (T_Cont argTys1, T_Cont argTys2) =>
		ListPair.allEq equal (argTys1, argTys2)
	   | (T_Any, T_Any) => true
	   | (T_CFun c_proto1, T_CFun c_proto2) =>
		c_proto1 = c_proto2
	   | _ => false
	  (* end case *))

  (* is a cast from srcTy to dstTy valid? *)
(* FIXME: casts between things that have uniform representation and T_Any are okay, but not
 * between raw types and T_Any!
 *)
    fun validCast {srcTy, dstTy} = (case (srcTy, dstTy)
	   of (T_Any, _) => true
	    | (_, T_Any) => true
	    | (ty1, ty2) => equal (ty1, ty2)
	  (* end case *))

  (* return a string representation of a type *)
    fun toString ty = let
	  fun xs2l toS (xs, l) = (case xs
		 of [] => l
		  | [x] => toS x :: l
		  | x::xs => toS x :: (List.foldr (fn (x, l) => "," :: toS x :: l) l xs)
		(* end case *))
	  val tys2l : t list * string list -> string list = xs2l toString
	  fun app2s (tyfn, tys) = concat(tyfn :: "<" :: tys2l (tys, [">"]))
	  in
	    case ty
	     of T_Param tv => BOMTyVar.toString tv
	      | T_Raw ty => RawTypes.toString ty
	      | T_Con(tyc, []) => BOMTyc.nameOf tyc
	      | T_Con(tyc, tys) => app2s(BOMTyc.nameOf tyc, tys)
	      | T_Record flds => let
		  fun fld2s (false, ty) = toString ty
		    | fld2s (true, ty) = "!" ^ toString ty
		  in
		    if List.exists #1 flds
		      then concat("{" :: xs2l fld2s (flds, ["}"]))
		      else concat("[" :: xs2l fld2s (flds, ["]"]))
		  end
	      | T_Packed flds => let
		  fun fld2s (loc, false, ty) = concat[Int.toString loc, ":", toString ty]
		    | fld2s (loc, true, ty) = concat[Int.toString loc, "!", toString ty]
		  in
		    concat("pack{" :: xs2l fld2s (flds, ["}"]))
		  end
	      | T_Fun(paramTys, exhTys, retTys) => let
		  val tys = (case retTys
			 of [] => [" -> ())"]
			  | _ => " -> " ::  tys2l (retTys, [")"])
			(* end case *))
		  val tys = " / " :: tys2l (exhTys, tys)
		  val tys = "(" :: tys2l (paramTys, tys)
		  in
		    concat tys
		  end
	      | T_Any => "any"
	      | T_CFun cp => CFunctions.protoToString cp
	    (* end case *)
	  end

  (* view a type as a function type *)
    fun asFunTy (T_Fun arg) = arg
      | asFunTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* get the return type(s) of a function type *)
    fun returnTy (T_Fun(_, _, ty)) = ty
      | returnTy (T_Cont _) = []
      | returnTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* view as tycon *)
    fun asTyc (T_Con(tyc, _)) = tyc
      | asTyc ty = raise Fail("expected tyc, but found " ^ toString ty)

  (* construct an immutable tuple type; nullary tuples are unit type and singleton
   * tuples have a direct representation.
   *)
    fun tupleTy [ty] = ty
      | tupleTy tys = T_Record(List.map (fn ty => (false, ty)) tys)

  (* select i'th type component from a tuple type *)
    fun select (T_Record flds, i) = #2(List.nth(flds, i))
      | select (T_Packed flds, i) = #3(List.nth(flds, i))
      | select (ty, _) = raise Fail("expected tuple type, but found " ^ toString ty)

  (* convert a C type into a BOM type with the same representation *)
    fun ctypeToBOM (CFunctions.PointerTy) = [T_Any]  (* FIXME: is this safe? *)
      | ctypeToBOM (CFunctions.BaseTy rTy) = [T_Raw rTy]
      | ctypeToBOM (CFunctions.VoidTy) = []

  (* some standard type constructors/types *)
    fun arrayTy ty = T_Con(BOMTyc.arrayTyc, [])
    fun vectorTy ty = T_Con(BOMTyc.vectorTyc, [])
    fun addrTy ty = T_Con(BOMTyc.addrTyc, [])
    val vprocTy = T_Con(BOMTyc.vprocTyc, [])
    val unitTy = T_Record[]

  end
