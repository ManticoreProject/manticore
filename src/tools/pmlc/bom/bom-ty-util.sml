(* bom-ty-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTyUtil : sig

    val kindToString : BOMTy.kind -> string
    val isKind : BOMTy.kind -> BOMTy.kind -> bool

    val equal : (BOMTy.ty * BOMTy.ty) -> bool
    val match : (BOMTy.ty * BOMTy.ty) -> bool
    val validCast : (BOMTy.ty * BOMTy.ty) -> bool

    val toString : BOMTy.ty -> string

  (* view a type as a function type *)
    val asFunTy : BOMTy.ty -> (BOMTy.ty list * BOMTy.ty list * BOMTy.ty list)

  (* get the return type(s) of a function type *)
    val returnTy : BOMTy.ty -> BOMTy.ty list

  (* view as tycon *)
    val asTyc : BOMTy.ty -> BOMTy.tyc

  (* select i'th type component from a tuple *)
    val select : BOMTy.ty * int -> BOMTy.ty

  (* return the type of a data constructor *)
    val typeOfDCon : BOMTy.data_con -> BOMTy.ty

  (* convert a C type into a BOM type with the same representation *)
    val ctypeToBOM : CFunctions.c_type -> BOMTy.ty list

  end = struct

    structure BTy = BOMTy

    fun kindToString k = (case k
	   of BTy.K_RAW => "RAW"
	    | BTy.K_BOXED => "BOXED"
	    | BTy.K_UNBOXED => "UNBOXED"
	    | BTy.K_UNIFORM => "UNIFORM"
	    | BTy.K_TYPE => "TYPE"
	  (* end case *))

  (* isKind k1 k2 --- returns true if k2 is a subkind of k1 *)
    fun isKind BTy.K_TYPE _ = true
      | isKind BTy.K_UNIFORM BTy.K_BOXED = true
      | isKind BTy.K_UNIFORM BTy.K_UNBOXED = true
      | isKind k1 k2 = (k1 = k2)

  (* compare types for equality *)
    fun equal (ty1, ty2) = (case (ty1, ty2)
          of (BTy.T_Param(BTy.TV a), BTy.T_Param(BTy.TV b)) => Stamp.same(a, b)
	   | (BTy.T_Raw rt1, BTy.T_Raw rt2) => rt1 = rt2
	   | (BTy.T_Con(tyc1, tys1), BTy.T_Con(tyc2, tys2)) =>
		BOMTyCon.sameTyc (tyc1, tyc2) andalso ListPair.allEq equal (tys1, tys2)
	   | (BTy.T_Tuple tys1, BTy.T_Tuple tys2) =>
		ListPair.allEq equal (tys1, tys2)
	   | (BTy.T_Record flds1, BTy.T_Record flds2) =>
		ListPair.allEq
		  (fn ((loc1, m1, ty1), (loc2, m2, ty2)) => (loc1 = loc2) andalso (m1 = m2) andalso equal(ty1, ty2))
		    (flds1, flds2)
	   | (BTy.T_Fun (argTys1, exhTys1, retTys1), 
	      BTy.T_Fun (argTys2, exhTys2, retTys2)) =>
		ListPair.allEq equal (argTys1, argTys2) andalso
		ListPair.allEq equal (exhTys1, exhTys2) andalso
		ListPair.allEq equal (retTys1, retTys2)
	   | (BTy.T_Cont argTys1, BTy.T_Cont argTys2) =>
		ListPair.allEq equal (argTys1, argTys2)
	   | (BTy.T_Array ty1, BTy.T_Array ty2) => equal (ty1, ty2)
	   | (BTy.T_Vector ty1, BTy.T_Vector ty2) => equal (ty1, ty2)
	   | (BTy.T_Addr ty1, BTy.T_Addr ty2) => equal (ty1, ty2)
	   | (BTy.T_Bignum, BTy.T_Bignum) => true
	   | (BTy.T_VProc, BTy.T_VProc) => true
	   | (BTy.T_Any, BTy.T_Any) => true
	   | (BTy.T_CFun c_proto1, BTy.T_CFun c_proto2) =>
		c_proto1 = c_proto2
	   | _ => false
	  (* end case *))

  (* does the first type "match" the second type (i.e., can values of the first
   * type be used wherever the second type is expected)?
   *)
    fun match (ty1, ty2) = (case (ty1, ty2)
	   of (_, BTy.T_Any) => true
	    | _ => equal(ty1, ty2)
	  (* end case *))
             
  (* is a cast from the first type to the second type valid? *)
    fun validCast (BTy.T_Any, _) = true
      | validCast (_, BTy.T_Any) = true
      | validCast (ty1, ty2) = match (ty1, ty2)

    fun toString ty = let
	  fun xs2l toS (xs, l) = (case xs
		 of [] => l
		  | [x] => toS x :: l
		  | x::xs => toS x :: (List.foldr (fn (x, l) => "," :: toS x :: l) l xs)
		(* end case *))
	  val tys2l : BOMTy.ty list * string list -> string list = xs2l toString
	  fun app2s (tyfn, tys) = concat(tyfn :: "<" :: tys2l (tys, [">"]))
	  in
	    case ty
	     of BTy.T_Param(BTy.TV s) => "'tv" ^ Stamp.toString s
	      | BTy.T_Raw ty => RawTypes.toString ty
	      | BTy.T_Con(tyc, []) => BOMTyCon.tycName tyc
	      | BTy.T_Con(tyc, tys) => app2s(BOMTyCon.tycName tyc, tys)
	      | BTy.T_Tuple tys => concat("[" :: tys2l(tys, ["]"]))
	      | BTy.T_Record flds => let
		  fun fld2s (loc, false, ty) = concat[Int.toString loc, ":", toString ty]
		    | fld2s (loc, true, ty) = concat[Int.toString loc, "!", toString ty]
		  in
		    concat("{" :: xs2l fld2s (flds, ["}"]))
		  end
	      | BTy.T_Fun(paramTys, exhTys, retTys) => let
		  val tys = (case retTys
			 of [] => [" -> ())"]
			  | _ => " -> " ::  tys2l (retTys, [")"])
			(* end case *))
		  val tys = " / " :: tys2l (exhTys, tys)
		  val tys = "(" :: tys2l (paramTys, tys)
		  in
		    concat tys
		  end
	      | BTy.T_Cont tys => app2s ("cont", tys)
	      | BTy.T_Array ty => app2s ("array", [ty])
	      | BTy.T_Vector ty => app2s ("vector", [ty])
	      | BTy.T_Addr ty => app2s ("addr", [ty])
	      | BTy.T_Bignum => "bignum"
	      | BTy.T_VProc => "vproc"
	      | BTy.T_Any => "any"
	      | BTy.T_CFun cp => CFunctions.protoToString cp
	    (* end case *)
	  end

  (* view a type as a function type *)
    fun asFunTy (BTy.T_Fun arg) = arg
      | asFunTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* get the return type(s) of a function type *)
    fun returnTy (BTy.T_Fun(_, _, ty)) = ty
      | returnTy (BTy.T_Cont _) = []
      | returnTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* view as tycon *)
    fun asTyc (BTy.T_Con(tyc, _)) = tyc
      | asTyc ty = raise Fail("expected tyc, but found " ^ toString ty)

  (* select i'th type component from a tuple/record *)
    fun select (BTy.T_Tuple tys, i) = List.nth(tys, i)
      | select (BTy.T_Record flds, i) = #3(List.nth(flds, i))
      | select (ty, _) = raise Fail("expected tuple type, but found " ^ toString ty)

  (* return the type of a data constructor *)
    fun typeOfDCon (BTy.DCon{myTyc, ...}) = BTy.T_Con(myTyc, [])  (* assuming monomorphic *)

  (* convert a C type into a BOM type with the same representation *)
    fun ctypeToBOM (CFunctions.PointerTy) = [BTy.T_Any]  (* FIXME: is this safe? *)
      | ctypeToBOM (CFunctions.BaseTy rTy) = [BTy.T_Raw rTy]
      | ctypeToBOM (CFunctions.VoidTy) = []

  end

