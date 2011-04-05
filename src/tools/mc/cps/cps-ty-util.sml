(* cps-ty-util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CPSTyUtil : sig

    val equal : (CPSTy.ty * CPSTy.ty) -> bool
    val match : (CPSTy.ty * CPSTy.ty) -> bool
    val soundMatch : (CPSTy.ty * CPSTy.ty) -> bool
    val validCast : (CPSTy.ty * CPSTy.ty) -> bool

    val toString : CPSTy.ty -> string

  (* view a type as a function type *)
    val asFunTy : CPSTy.ty -> (CPSTy.ty list * CPSTy.ty list)

  (* get the return type(s) of a function type *)
    val returnTy : CPSTy.ty -> CPSTy.ty

  (* select i'th type component from a tuple *)
    val select : CPSTy.ty * int -> CPSTy.ty

  (* convert a C type into a CPS type with the same representation *)
    val ctypeToCPS : CFunctions.c_type -> CPSTy.ty list

  (* return the kind of a type *)
    val kindOf : CPSTy.ty -> CPSTy.kind

  (* isKind k1 k2 --- returns true if k2 is a subkind of k1 *)
    val isKind : CPSTy.kind -> CPSTy.kind -> bool
  end = struct

    structure CTy = CPSTy

    fun kindToString k = (case k
	   of CTy.K_RAW => "RAW"
	    | CTy.K_BOXED => "BOXED"
	    | CTy.K_UNBOXED => "UNBOXED"
	    | CTy.K_UNIFORM => "UNIFORM"
	    | CTy.K_TYPE => "TYPE"
	  (* end case *))

    fun isKind CTy.K_TYPE _ = true
      | isKind CTy.K_UNIFORM CTy.K_BOXED = true
      | isKind CTy.K_UNIFORM CTy.K_UNBOXED = true
      | isKind k1 k2 = (k1 = k2)

    fun kindOf CTy.T_Any = CTy.K_UNIFORM
      | kindOf (CTy.T_Enum _) = CTy.K_UNBOXED
      | kindOf (CTy.T_Raw _) = CTy.K_RAW
      | kindOf (CTy.T_Tuple _) = CTy.K_BOXED
      | kindOf (CTy.T_Addr _) = CTy.K_TYPE
      | kindOf (CTy.T_Fun _) = CTy.K_BOXED
      | kindOf (CTy.T_CFun _) = CTy.K_UNIFORM
      | kindOf CTy.T_VProc = CTy.K_RAW

  (* compare types for equality *)
    fun equal (ty1, ty2) = (case (ty1, ty2)
          of (CTy.T_Any, CTy.T_Any) => true
	   | (CTy.T_Enum w1, CTy.T_Enum w2) => w1 = w2
	   | (CTy.T_Raw rt1, CTy.T_Raw rt2) => rt1 = rt2
	   | (CTy.T_Tuple (m1, tys1), CTy.T_Tuple (m2, tys2)) =>
		m1 = m2 andalso
		ListPair.allEq equal (tys1, tys2)
	   | (CTy.T_Addr ty1, CTy.T_Addr ty2) => equal (ty1, ty2)
	   | (CTy.T_Fun (argTys1, contTys1), 
	      CTy.T_Fun (argTys2, contTys2)) =>
		ListPair.allEq equal (argTys1, argTys2) andalso
		ListPair.allEq equal (contTys1, contTys2)
	   | (CTy.T_CFun c_proto1, CTy.T_CFun c_proto2) =>
		c_proto1 = c_proto2
	   | (CTy.T_VProc, CTy.T_VProc) => true
           | _ => false
	  (* end case *))

  (* does the first type "match" the second type (i.e., can values of the first
   * type be used wherever the second type is expected)?
   *)
    fun match' (ty1, ty2, sound) = (case (ty1, ty2)
	   of (CTy.T_Addr ty1, CTy.T_Addr ty2) => equal(ty1, ty2)
	    | (CTy.T_Raw rty1, CTy.T_Raw rty2) => (rty1 = rty2)
	    | (fromTy, CTy.T_Any) => isKind CTy.K_UNIFORM (kindOf fromTy)
	  (* the following shouldn't be here, since it isn't really sound, but we need it
	   * to handle surface-language polymorphism, which is translated to T_Any.
	   *)
	    | (CTy.T_Any, toTy) => if sound
                                   then false
                                   else isKind CTy.K_UNIFORM (kindOf toTy)
	    | (CTy.T_Enum w1, CTy.T_Enum w2) => (w1 <= w2)
	    | (CTy.T_Enum _, CTy.T_Tuple _) => true
	    | (CTy.T_Tuple(isMut1, tys1), CTy.T_Tuple(isMut2, tys2)) =>
		(isMut1 orelse not isMut2)
		andalso ListPair.allEq (fn (x,y) => match' (x,y,sound))
                                       (tys1, tys2)
	    | (CTy.T_Fun(argTys1, contTys1), CTy.T_Fun(argTys2, contTys2)) =>
	      (* Note contravariance for arguments! *)
		ListPair.allEq (fn (x,y) => match' (x,y,sound)) (argTys2, argTys1)
                andalso ListPair.allEq (fn (x,y) => match' (x,y,sound))
                                       (contTys2, contTys1)
	    | _ => equal(ty1, ty2)
	  (* end case *))

    fun match (ty1, ty2) = match' (ty1, ty2, false)
    fun soundMatch (ty1, ty2) = match' (ty1, ty2, true)
             
  (* is a cast from the first type to the second type valid? *)
    fun validCast (CTy.T_Raw rty1, CTy.T_Raw rty2) = (rty1 = rty2)
      | validCast (ty1, ty2) =
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
	     of CTy.T_Any => "any"
	      | CTy.T_Enum w => concat["enum(", Word.fmt StringCvt.DEC w, ")"]
	      | CTy.T_Raw ty => RawTypes.toString ty
	      | CTy.T_Tuple(false, tys) => concat("[" :: tys2l(tys, ["]"]))
	      | CTy.T_Tuple(true, tys) => concat("![" :: tys2l(tys, ["]"]))
	      | CTy.T_Addr ty => concat["addr(", toString ty, ")"]
	      | CTy.T_Fun(tys, []) => concat("cont(" :: tys2l(tys, [")"]))
	      | CTy.T_Fun(tys1, tys2) => concat("fun(" :: tys2l(tys1, " / " :: tys2l(tys2, [")"])))
	      | CTy.T_CFun cp => CFunctions.protoToString cp
	      | CTy.T_VProc => "vproc"
	    (* end case *)
	  end

  (* view a type as a function type *)
    fun asFunTy (CTy.T_Fun arg) = arg
      | asFunTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* extract the return continuation type from a function type *)
    fun returnTy (CTy.T_Fun(_, ty::_)) = ty
      | returnTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* select i'th type component from a tuple *)
    fun select (CTy.T_Tuple(_, tys), i) = List.nth(tys, i)
      | select (ty, _) = raise Fail("expected tuple type, but found " ^ toString ty)

  (* convert a C type into a CPS type with the same representation *)
    fun ctypeToCPS (CFunctions.PointerTy) = [CTy.T_Any]  (* FIXME: is this safe? *)
      | ctypeToCPS (CFunctions.BaseTy rTy) = [CTy.T_Raw rTy]
      | ctypeToCPS (CFunctions.VoidTy) = []

  end
