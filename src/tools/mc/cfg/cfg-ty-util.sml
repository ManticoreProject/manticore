(* cfg-ty-util.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Types for the first-order CPS representation.
 *)

structure CFGTyUtil : sig

    val hasUniformRep : CFGTy.ty -> bool

    val kindOf : CFGTy.ty -> CFGTy.kind

    val equal : (CFGTy.ty * CFGTy.ty) -> bool
    val match : (CFGTy.ty * CFGTy.ty) -> bool
    val validCast : (CFGTy.ty * CFGTy.ty) -> bool

    val kindToString : CFGTy.kind -> string
    val toString : CFGTy.ty -> string

    val stdContTy : CFGTy.ty * CFGTy.ty list -> CFGTy.ty
    val kwnContTy : CFGTy.ty * CFGTy.ty list -> CFGTy.ty

  (* select i'th type component from a tuple *)
    val select : CFGTy.ty * int -> CFGTy.ty

  end = struct

    structure CTy = CFGTy

    fun kindToString k = (case k
	   of CTy.K_RAW => "RAW"
	    | CTy.K_BOXED => "BOXED"
	    | CTy.K_UNBOXED => "UNBOXED"
	    | CTy.K_UNIFORM => "UNIFORM"
	    | CTy.K_ANY => "ANY"
	    | CTy.K_TYPE => "TYPE"
	  (* end case *))

  (* isKind k1 k2 --- returns true if k2 is a subkind of k1 *)
    fun isKind CTy.K_TYPE _ = true
      | isKind CTy.K_ANY CTy.K_UNIFORM = true
      | isKind CTy.K_ANY CTy.K_BOXED = true
      | isKind CTy.K_ANY CTy.K_UNBOXED = true
      | isKind CTy.K_UNIFORM CTy.K_BOXED = true
      | isKind CTy.K_UNIFORM CTy.K_UNBOXED = true
      | isKind k1 k2 = (k1 = k2)

    fun kindOf CTy.T_Any = CTy.K_ANY
      | kindOf (CTy.T_Enum _) = CTy.K_UNBOXED
      | kindOf (CTy.T_Raw _) = CTy.K_RAW
      | kindOf (CTy.T_Tuple _) = CTy.K_BOXED
      | kindOf (CTy.T_OpenTuple _) = CTy.K_BOXED
      | kindOf (CTy.T_Addr _) = CTy.K_TYPE
      | kindOf (CTy.T_CFun _) = CTy.K_UNIFORM
      | kindOf CTy.T_VProc = CTy.K_RAW
      | kindOf (CTy.T_StdFun _) = CTy.K_RAW
      | kindOf (CTy.T_StdCont _) = CTy.K_RAW
      | kindOf (CTy.T_KnownFunc _) = CTy.K_TYPE

  (* compare types for equality *)
    fun equal (ty1, ty2) = (case (ty1, ty2)
	   of (CTy.T_Any, CTy.T_Any) => true
            | (CTy.T_Enum w1, CTy.T_Enum w2) => w1 = w2
            | (CTy.T_Raw rty1, CTy.T_Raw rty2) => rty1 = rty2
            | (CTy.T_Tuple(mut1, tys1), CTy.T_Tuple(mut2, tys2)) =>
                 mut1 = mut2 andalso equalList (tys1, tys2)
            | (CTy.T_OpenTuple tys1, CTy.T_OpenTuple tys2) => 
                 equalList (tys1, tys2)
	    | (CTy.T_Addr ty1, CTy.T_Addr ty2) => equal (ty1, ty2)
	    | (CTy.T_CFun c_proto1, CTy.T_CFun c_proto2) => c_proto1 = c_proto2
	    | (CTy.T_VProc, CTy.T_VProc) => true
            | (CTy.T_StdFun{clos = clos1, args = args1, ret = ret1, exh = exh1},
               CTy.T_StdFun{clos = clos2, args = args2, ret = ret2, exh = exh2}) =>
                  equal (clos1, clos2) andalso equalList (args1, args2) andalso
                  equal (ret1, ret2) andalso equal (exh1, exh2)
            | (CTy.T_StdCont{clos = clos1, args = args1}, 
               CTy.T_StdCont{clos = clos2, args = args2}) =>
                  equal (clos1, clos2) andalso equalList (args1, args2)
            | (CTy.T_KnownFunc{clos = clos1, args = args1}, 
               CTy.T_KnownFunc{clos = clos2, args = args2}) => 
                  equal (clos1, clos2) andalso equalList (args1, args2)
            | _ => false
	  (* end case *))
    and equalList (tys1, tys2) = ListPair.allEq equal (tys1, tys2)

  (* return true if the type has a single-word uniform representation; this includes
   * anything represented by a pointer or a tagged integer.
   *)
    fun hasUniformRep ty = isKind CTy.K_UNIFORM (kindOf ty)

  (* does the first type "match" the second type (i.e., can values of the first
   * type be used wherever the second type is expected)?
   *)
    fun match (fromTy, toTy) = (case (fromTy, toTy)
           of (CTy.T_Addr ty1, CTy.T_Addr ty2) => equal(ty1, ty2)
	    | (CTy.T_Raw rty1, CTy.T_Raw rty2) => (rty1 = rty2)
	    | (fromTy, CTy.T_Any) => isKind CTy.K_ANY (kindOf fromTy)
	  (* the following shouldn't be here, since it isn't really sound, but we need it
	   * to handle surface-language polymorphism, which is translated to T_Any.
	   *)
	    | (CTy.T_Any, toTy) => isKind CTy.K_ANY (kindOf toTy)
	    | (CTy.T_Enum w1, CTy.T_Enum w2) => (w1 <= w2)
	    | (CTy.T_Enum _, CTy.T_Tuple _) => true
	    | (CTy.T_Tuple(isMut1, tys1), CTy.T_Tuple(isMut2, tys2)) =>
		(isMut1 orelse not isMut2)
		andalso ListPair.allEq match (tys1, tys2)
	    | (CTy.T_Tuple(false, tys1), CTy.T_OpenTuple tys2) => let
		fun ok (_, []) = true
		  | ok (ty1::r1, ty2::r2) = match(ty1, ty2) andalso ok(r1, r2)
		  | ok ([], _) = false
		in
		  ok (tys1, tys2)
		end
            | (CTy.T_OpenTuple tys1, CTy.T_OpenTuple tys2) => let
		fun ok (_, []) = true
		  | ok (ty1::r1, ty2::r2) = match(ty1, ty2) andalso ok(r1, r2)
		  | ok ([], _) = false
		in
		  ok (tys1, tys2)
		end
            | (CTy.T_StdFun{clos = clos1, args = args1, ret = ret1, exh = exh1},
               CTy.T_StdFun{clos = clos2, args = args2, ret = ret2, exh = exh2}) =>
              (* Note contravariance for arguments! *)
                  (match (clos2, clos1) orelse validCast (clos2, clos1)) andalso
                  ListPair.allEq match (args2, args1) andalso
                  match (ret2, ret1) andalso
                  match (exh2, exh1)
            | (CTy.T_StdCont{clos = clos1, args = args1}, 
               CTy.T_StdCont{clos = clos2, args = args2}) =>
              (* Note contravariance for arguments! *)
                  (match (clos2, clos1) orelse validCast (clos2, clos1)) andalso
                  ListPair.allEq match (args2, args1)
            | (CTy.T_KnownFunc{clos = clos1, args = args1},
               CTy.T_KnownFunc{clos = clos2, args = args2}) => 
              (* Note contravariance for arguments! *)
                  (match (clos2, clos1) orelse validCast (clos2, clos1)) andalso
                  ListPair.allEq match (args2, args1)
            | _ => equal (fromTy, toTy)
	  (* end case *))

  (* is a cast from the first type to the second type valid? *)
    and validCast (CTy.T_Raw rty1, CTy.T_Raw rty2) = (rty1 = rty2)
      | validCast (ty1, ty2) =
	  isKind (kindOf ty1) (kindOf ty2)
	    orelse match (ty1, ty2)

    fun toString ty = let
	  fun tys2l ([], l) = l
	    | tys2l ([ty], l) = toString ty :: l
	    | tys2l (ty::tys, l) =
		toString ty ::
		  (List.foldr (fn (ty, l) => "," :: toString ty :: l) l tys)
	  fun args2s tys = concat(tys2l(tys, []))
	  in
	    case ty
	     of CTy.T_Any => "any"
	      | CTy.T_Enum w => concat["enum(", Word.fmt StringCvt.DEC w, ")"]
	      | CTy.T_Raw ty => RawTypes.toString ty
	      | CTy.T_Tuple(false, tys) => concat("[" :: tys2l(tys, ["]"]))
	      | CTy.T_Tuple(true, tys) => concat("![" :: tys2l(tys, ["]"]))
	      | CTy.T_OpenTuple tys => concat("[" :: tys2l(tys, [",...]"]))
	      | CTy.T_Addr ty => concat["addr(", toString ty, ")"]
	      | CTy.T_CFun proto => CFunctions.protoToString proto
	      | CTy.T_VProc => "vproc"
	      | CTy.T_StdFun{clos, args, ret, exh} => concat[
		    "fun(", toString clos, "/", args2s args, "/",
		    toString ret, "/", toString exh, ")"
		  ]
	      | CTy.T_StdCont{clos, args} => concat[
		    "cont(", toString clos, "/", args2s args, ")"
		  ]
	      | CTy.T_KnownFunc{clos, args} => concat[
                    "kfun(", toString clos, "/", args2s args, ")"
                  ]
	    (* end case *)
	  end

    fun stdContTy (cpTy, argTy) = let
	  val cpTy = (case cpTy
		 of CTy.T_Any => CTy.T_StdCont{
			clos=CTy.T_OpenTuple[CTy.T_StdCont{clos=CTy.T_Any, args=argTy}],
			args=argTy
		      }
		  | CTy.T_StdCont _ => cpTy
		  | ty => raise Fail(concat[
			"stdContTy(", toString cpTy, "/",
			String.concatWith "," (List.map toString argTy), ")"
		      ])
		(* end case *))
	  in
	    cpTy
	  end

    fun kwnContTy (cpTy, argTy) = let
	  val cpTy = (case cpTy
		 of CTy.T_Any => CTy.T_KnownFunc{
			clos=CTy.T_OpenTuple[CTy.T_KnownFunc{clos=CTy.T_Any, args=argTy}],
			args=argTy
		      }
		  | CTy.T_KnownFunc _ => cpTy
		  | ty => raise Fail(concat[
			"kwnContTy(", toString cpTy, "/",
			String.concatWith "," (List.map toString argTy), ")"
		      ])
		(* end case *))
	  in
	    cpTy
	  end

    fun select (ty, i) = let
	  fun err () = raise Fail(concat["selectTy(", Int.toString i, ", ", toString ty, ")"])
	  fun sel (_, []) = err()
	    | sel (0, ty::r) = ty
	    | sel (i, _::r) = sel(i-1, r)
	  in
	    case ty
	     of CTy.T_Tuple(_, tys) => sel(i, tys)
	      | CTy.T_OpenTuple tys => sel(i, tys)
	      | _ => err()
	    (* end case *)
	  end

  end
