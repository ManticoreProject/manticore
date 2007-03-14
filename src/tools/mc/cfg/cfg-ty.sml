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
      | T_OpenTuple of ty list	(* a tuple of unknown size, where we know the prefix. *)
    (* function/continuation types.  The type specifies the calling convention.  These
     * types should be used for labels and code addresses.
     *)
      | T_StdFun of {clos : ty, arg : ty, ret : ty, exh : ty}
      | T_StdCont of {clos : ty, arg : ty}
      | T_Code of ty list	(* includes both known functions and blocks *)

    val unitTy = T_Enum(0w0)
    val boolTy = T_Enum(0w1)	(* false = 0, true = 1 *)

    fun equals (ty1, ty2) = (case (ty1, ty2)
	   of (T_Any, T_Any) => true
            | (T_Enum w1, T_Enum w2) => (w1 = w2)
            | (T_Raw rty1, T_Raw rty2) => (rty1 = rty2)
            | (T_Wrap rty1, T_Wrap rty2) => (rty1 = rty2)
            | (T_Tuple ty1s, T_Tuple ty2s) => ListPair.allEq equals (ty1s, ty2s)
            | (T_OpenTuple ty1s, T_OpenTuple ty2s) => ListPair.allEq equals (ty1s, ty2s)
            | (T_StdFun {clos = clos1, arg = arg1, ret = ret1, exh = exh1},
               T_StdFun {clos = clos2, arg = arg2, ret = ret2, exh = exh2}) =>
                  equals (clos1, clos2) andalso
                  equals (arg1, arg2) andalso
                  equals (ret1, ret2) andalso
                  equals (exh1, exh2)
            | (T_StdCont {clos = clos1, arg = arg1}, 
               T_StdCont {clos = clos2, arg = arg2}) =>
                  equals (clos1, clos2) andalso
                  equals (arg1, arg2)
            | (T_Code ty1s, T_Code ty2s) => ListPair.allEq equals (ty1s, ty2s)
            | _ => false
	  (* end case *))

  (* return true if the type has a single-word uniform representation; this includes
   * anything represented by a pointer or a tagged integer.
   *)
    fun hasUniformRep ty = (case ty 
           of T_Raw _ => false
            | _ => true
	  (* end case *))

    fun isBoxed ty = (case ty 
           of T_Any => false
            | T_Enum _ => false
            | T_Raw _ => false
            | T_Wrap _ => true
            | T_Tuple _ => true
            | T_OpenTuple _ => true
            | T_StdFun _ => true
            | T_StdCont _ => true
            | T_Code _ => true
	  (* end case *))

    fun isValidCast (fromTy, toTy) = (case (fromTy, toTy)
           of (T_Any, T_Code _) => false
	    | (T_Code _, T_Any) => false
	    | (T_Any, toTy) => hasUniformRep toTy
            | (fromTy, T_Any) => hasUniformRep fromTy
            | (T_OpenTuple ty1s, T_OpenTuple ty2s) =>
                  ListPair.all isValidCast (ty1s, ty2s)
            | (T_StdFun {clos = clos1, arg = arg1, ret = ret1, exh = exh1},
               T_StdFun {clos = clos2, arg = arg2, ret = ret2, exh = exh2}) => true
            | (T_StdCont {clos = clos1, arg = arg1},
               T_StdCont {clos = clos2, arg = arg2}) => true
            | _ => equals (fromTy, toTy)
	  (* end case *))

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
