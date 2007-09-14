(* bom-ty-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTyUtil : sig

    val kindToString : BOMTy.kind -> string
    val isKind : BOMTy.kind -> BOMTy.kind -> bool
    val kindOf : BOMTy.ty -> BOMTy.kind

    val equal : (BOMTy.ty * BOMTy.ty) -> bool
    val match : (BOMTy.ty * BOMTy.ty) -> bool
    val validCast : (BOMTy.ty * BOMTy.ty) -> bool

    val toString : BOMTy.ty -> string

  (* Construct a wrapped type from a raw type *)
    val wrap : BOMTy.ty -> BOMTy.ty

  (* Unwrap a wrapped type returning a raw type *)
    val unwrap : BOMTy.ty -> BOMTy.ty

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

    fun kindOf BTy.T_Any = BTy.K_UNIFORM
      | kindOf (BTy.T_Enum _) = BTy.K_UNBOXED
      | kindOf (BTy.T_Raw _) = BTy.K_RAW
      | kindOf (BTy.T_Tuple _) = BTy.K_BOXED
      | kindOf (BTy.T_Addr _) = BTy.K_TYPE
      | kindOf (BTy.T_Fun _) = BTy.K_BOXED
      | kindOf (BTy.T_Cont _) = BTy.K_BOXED
      | kindOf (BTy.T_CFun _) = BTy.K_UNIFORM
      | kindOf BTy.T_VProc = BTy.K_UNIFORM
      | kindOf (BTy.T_TyCon(BTy.DataTyc{name, rep, ...})) = (case !rep
	   of SOME ty => kindOf ty
	    | NONE => BTy.K_UNIFORM
	  (* end case *))
      | kindOf (BTy.T_TyCon(BTy.AbsTyc _)) = BTy.K_UNIFORM

  (* compare types for equality *)
    fun equal (ty1, ty2) = (case (ty1, ty2)
	   of (BTy.T_Any, BTy.T_Any) => true
	   | (BTy.T_Enum w1, BTy.T_Enum w2) => w1 = w2
	   | (BTy.T_Raw rt1, BTy.T_Raw rt2) => rt1 = rt2
	   | (BTy.T_Tuple (m1, tys1), BTy.T_Tuple (m2, tys2)) =>
		m1 = m2 andalso
		ListPair.allEq equal (tys1, tys2)
	   | (BTy.T_Addr ty1, BTy.T_Addr ty2) => equal (ty1, ty2)
	   | (BTy.T_Fun (argTys1, exhTys1, retTys1), 
	      BTy.T_Fun (argTys2, exhTys2, retTys2)) =>
		ListPair.allEq equal (argTys1, argTys2) andalso
		ListPair.allEq equal (exhTys1, exhTys2) andalso
		ListPair.allEq equal (retTys1, retTys2)
	   | (BTy.T_Cont argTys1, BTy.T_Cont argTys2) =>
		ListPair.allEq equal (argTys1, argTys2)
	   | (BTy.T_CFun c_proto1, BTy.T_CFun c_proto2) =>
		c_proto1 = c_proto2
	   | (BTy.T_VProc, BTy.T_VProc) => true
	   | (BTy.T_TyCon tyc1, BTy.T_TyCon tyc2) => eqTyc (tyc1, tyc2)
	   | _ => false
	  (* end case *))
	  
    and eqTyc (tyc1, tyc2) = (case (tyc1, tyc2)
	   of (BTy.DataTyc {stamp = stamp1, ...}, BTy.DataTyc {stamp = stamp2, ...}) =>
		Stamp.same (stamp1, stamp2)
	    | (BTy.AbsTyc {stamp = stamp1, ...}, BTy.AbsTyc {stamp = stamp2, ...}) =>
		Stamp.same (stamp1, stamp2)
	    | _ => false
	  (* end case *))

  (* does the first type "match" the second type (i.e., can values of the first
   * type be used wherever the second type is expected)?
   *)
    fun match (ty1, ty2) = (case (ty1, ty2)
	   of (BTy.T_Addr ty1, BTy.T_Addr ty2) => equal(ty1, ty2)
	    | (BTy.T_Addr _, _) => false
	    | (_, BTy.T_Addr _) => false
	    | (BTy.T_Raw rty1, BTy.T_Raw rty2) => (rty1 = rty2)
	    | (BTy.T_Raw _, _) => false
	    | (_, BTy.T_Raw _) => false
	    | (_, BTy.T_Any) => true
	    | (BTy.T_Enum w1, BTy.T_Enum w2) => (w1 <= w2)
	    | (BTy.T_Enum w, BTy.T_TyCon(BTy.DataTyc{nNullary, ...})) => (w < Word.fromInt nNullary)
            | (ty1, ty2 as BTy.T_TyCon (BTy.DataTyc {cons, ...})) =>
                equal(ty1, ty2) orelse
                List.exists (fn BTy.DCon {rep, argTy, ...} =>
                             let
                                val matchTy =
                                   case rep of
                                      BTy.Transparent => (case argTy of [ty] => ty)
                                    | BTy.Tuple => BTy.T_Tuple (false, argTy)
                                    | BTy.TaggedTuple tag => BTy.T_Tuple (false, (BTy.T_Enum tag) :: argTy)
                             in
                                equal (matchTy, ty1)
                             end) (!cons)
	    | (BTy.T_Tuple(isMut1, tys1), BTy.T_Tuple(isMut2, tys2)) =>
		(isMut1 orelse not isMut2)
		andalso ListPair.allEq match (tys1, tys2)
	    | (BTy.T_Fun(argTys1, exhTys1, retTys1), BTy.T_Fun(argTys2, exhTys2, retTys2)) =>
	      (* Note contravariance for arguments! *)
		ListPair.allEq match (argTys2, argTys1)
                andalso ListPair.allEq match (exhTys2, exhTys1)
		andalso ListPair.allEq match (retTys1, retTys2)
	    | (BTy.T_Cont argTys1, BTy.T_Cont argTys2) =>
	      (* Note contravariance for arguments! *)
		ListPair.allEq match (argTys2, argTys1)
	    | _ => equal(ty1, ty2)
	  (* end case *))
             
  (* is a cast from the first type to the second type valid? *)
    fun validCast (ty1, ty2) =
	  isKind (kindOf ty1) (kindOf ty2)
	    orelse match (ty1, ty2)

    fun toString ty = let
	  fun tys2l ([], l) = l
	    | tys2l ([ty], l) = toString ty :: l
	    | tys2l (ty::tys, l) =
		toString ty ::
		  (List.foldr (fn (ty, l) => "," :: toString ty :: l) l tys)
	  in
	    case ty
	     of BTy.T_Any => "any"
	      | BTy.T_Enum w => concat["enum(", Word.fmt StringCvt.DEC w, ")"]
	      | BTy.T_Raw ty => RawTypes.toString ty
	      | BTy.T_Tuple(false, tys) => concat("[" :: tys2l(tys, ["]"]))
	      | BTy.T_Tuple(true, tys) => concat("![" :: tys2l(tys, ["]"]))
	      | BTy.T_Addr ty => concat["addr(", toString ty, ")"]
	      | BTy.T_Fun(paramTys, exhTys, retTys) => let
		  fun f1 [] = "-;" :: f2 exhTys
		    | f1 [ty] = toString ty :: ";" :: f2 exhTys
		    | f1 (ty::tys) = toString ty :: "," :: f1 tys
		  and f2 [] = "-) -> (" :: f3 retTys
		    | f2 [ty] = toString ty :: ") -> (" :: f3 retTys
		    | f2 (ty::tys) = toString ty :: "," :: f2 tys
		  and f3 [] = [")"]
		    | f3 [ty] = [toString ty, ")"]
		    | f3 (ty::tys) = toString ty :: "," :: f3 tys
		  in
		    concat("(" :: f1 paramTys)
		  end
	      | BTy.T_Cont tys => concat("cont(" :: tys2l(tys, [")"]))
	      | BTy.T_CFun cp => CFunctions.protoToString cp
	      | BTy.T_VProc=> "vproc"
	      | BTy.T_TyCon(BTy.DataTyc{name, ...}) => name
	      | BTy.T_TyCon(BTy.AbsTyc{name, ...}) => name
	    (* end case *)
	  end

  (* wrapped raw values are stored in tuples *)
    fun wrap (ty as BTy.T_Raw _) = BTy.T_Tuple(false, [ty])
      | wrap ty = raise Fail(concat["wrap(", toString ty, ")"])

    fun unwrap (BTy.T_Tuple(false, [ty as BTy.T_Raw _])) = ty
      | unwrap ty = raise Fail(concat["unwrap(", toString ty, ")"])

  (* view a type as a function type *)
    fun asFunTy (BTy.T_Fun arg) = arg
      | asFunTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* get the return type(s) of a function type *)
    fun returnTy (BTy.T_Fun(_, _, ty)) = ty
      | returnTy (BTy.T_Cont _) = []
      | returnTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* view as tycon *)
    fun asTyc (BTy.T_TyCon tyc) = tyc
      | asTyc ty = raise Fail("expected tyc, but found " ^ toString ty)

  (* select i'th type component from a tuple *)
    fun select (BTy.T_Tuple(_, tys), i) = List.nth(tys, i)
      | select (ty, _) = raise Fail("expected tuple type, but found " ^ toString ty)

  (* return the type of a data constructor *)
    fun typeOfDCon (BTy.DCon{myTyc, ...}) = BTy.T_TyCon myTyc

  (* convert a C type into a BOM type with the same representation *)
    fun ctypeToBOM (CFunctions.PointerTy) = [BTy.T_Any]  (* FIXME: is this safe? *)
      | ctypeToBOM (CFunctions.BaseTy rTy) = [BTy.T_Raw rTy]
      | ctypeToBOM (CFunctions.VoidTy) = []

  end
