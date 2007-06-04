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
      = T_Any				(* unknown type; uniform representation *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_Wrap of raw_ty		(* boxed raw value *)
      | T_Tuple of bool * ty list	(* heap-allocated tuple; the boolean is true for *)
					(* mutable tuples *)
      | T_OpenTuple of ty list		(* an immutable tuple of unknown size, where we know the prefix. *)
      | T_Addr of ty			(* address of a tuple's field *)
      | T_CFun of CFunctions.c_proto	(* a C function prototype *)
      | T_VProc				(* address of runtime vproc structure *)
    (* function/continuation types.  The type specifies the calling convention.  These
     * types should be used for labels and code addresses.
     *)
      | T_StdFun of {clos : ty, args : ty list, ret : ty, exh : ty}
      | T_StdCont of {clos : ty, args : ty list}
      | T_Code of ty list	(* includes both known functions and blocks *)

    val unitTy = T_Enum(0w0)
    val boolTy = T_Enum(0w1)	(* false = 0, true = 1 *)

    fun equal (ty1, ty2) = (case (ty1, ty2)
	   of (T_Any, T_Any) => true
            | (T_Enum w1, T_Enum w2) => (w1 = w2)
            | (T_Raw rty1, T_Raw rty2) => (rty1 = rty2)
            | (T_Wrap rty1, T_Wrap rty2) => (rty1 = rty2)
            | (T_Tuple(mut1, ty1s), T_Tuple(mut2, ty2s)) =>
		(mut1 = mut2) andalso equalList (ty1s, ty2s)
            | (T_OpenTuple ty1s, T_OpenTuple ty2s) => equalList (ty1s, ty2s)
	    | (T_Addr ty1, T_Addr ty2) => equal(ty1, ty2)
	    | (T_CFun proto1, T_CFun proto2) => (proto1 = proto2)
	    | (T_VProc, T_VProc) => true
            | (T_StdFun{clos = clos1, args = args1, ret = ret1, exh = exh1},
               T_StdFun{clos = clos2, args = args2, ret = ret2, exh = exh2}) =>
                  equal (clos1, clos2) andalso
                  equalList (args1, args2) andalso
                  equal (ret1, ret2) andalso
                  equal (exh1, exh2)
            | (T_StdCont{clos = clos1, args = args1}, 
               T_StdCont{clos = clos2, args = args2}) =>
                  equal (clos1, clos2) andalso equalList (args1, args2)
            | (T_Code ty1s, T_Code ty2s) => equalList (ty1s, ty2s)
            | _ => false
	  (* end case *))

    and equalList (tys1, tys2) = ListPair.allEq equal (tys1, tys2)
  (* return true if the type has a single-word uniform representation; this includes
   * anything represented by a pointer or a tagged integer.
   *)
    fun hasUniformRep ty = (case ty 
           of T_Raw _ => false
            | _ => true
	  (* end case *))

  (* return true if the type is represented by a pointer (including pointers
   * that lie outside the heap).
   *)
    fun isBoxed ty = (case ty 
           of T_Any => false
            | T_Enum _ => false
            | T_Raw _ => false
            | T_Wrap _ => true
            | T_Tuple _ => true
            | T_OpenTuple _ => true
	    | T_Addr _ => raise Fail "isBoxed(addr)"
	    | T_CFun _ => true
	    | T_VProc => true
            | T_StdFun _ => true
            | T_StdCont _ => true
            | T_Code _ => true
	  (* end case *))

    fun isValidCast (fromTy, toTy) = (case (fromTy, toTy)
           of (T_Any, T_Code _) => false
	    | (T_Code _, T_Any) => false
	    | (T_Any, T_Addr _) => false
	    | (T_Addr _, T_Any) => false
	    | (T_Any, toTy) => hasUniformRep toTy
            | (fromTy, T_Any) => hasUniformRep fromTy
            | (T_OpenTuple ty1s, T_OpenTuple ty2s) => ListPair.all isValidCast (ty1s, ty2s)
            | (T_StdFun _, T_StdFun _) => true
            | (T_StdCont _, T_StdCont _) => true
            | _ => equal (fromTy, toTy)
	  (* end case *))

    fun toString ty = let
	  fun tys2l ([], l) = l
	    | tys2l ([ty], l) = toString ty :: l
	    | tys2l (ty::tys, l) =
		toString ty ::
		  (List.foldr (fn (ty, l) => "," :: toString ty :: l) l tys)
	  fun args2s [] = "[]"
	    | args2s [ty] = toString ty
	    | args2s tys = concat("[" :: tys2l(tys, ["]"]))
	  in
	    case ty
	     of T_Any => "any"
	      | T_Enum w => concat["enum(", Word.fmt StringCvt.DEC w, ")"]
	      | T_Raw ty => RawTypes.toString ty
	      | T_Wrap ty => concat["wrap(", RawTypes.toString ty, ")"]
	      | T_Tuple(false, tys) => concat("(" :: tys2l(tys, [")"]))
	      | T_Tuple(true, tys) => concat("!(" :: tys2l(tys, [")"]))
	      | T_OpenTuple tys => concat("(" :: tys2l(tys, [",...)"]))
	      | T_Addr ty => concat["addr(", toString ty, ")"]
	      | T_CFun proto => CFunctions.protoToString proto
	      | T_VProc => "vproc"
	      | T_StdFun{clos, args, ret, exh} => concat[
		    "fun(", toString clos, ",", args2s args, ",",
		    toString ret, ",", toString exh, ")"
		  ]
	      | T_StdCont{clos, args} =>  concat[
		    "cont(", toString clos, ",", args2s args, ")"
		  ]
	      | T_Code tys => concat("code(" :: tys2l(tys, [")"]))
	    (* end case *)
	  end

    fun stdContTy (cpTy, argTy) = let
	  val cpTy = (case cpTy
		 of T_Any => T_StdCont{
			clos=T_OpenTuple[T_StdCont{clos=T_Any, args=argTy}],
			args=argTy
		      }
		  | T_StdCont _ => cpTy
		  | ty => raise Fail(concat[
			"stdContTy(", toString cpTy, ", ", "[",
			String.concatWith "," (List.map toString argTy), "])"
		      ])
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
	     of T_Tuple(_, tys) => sel(i, tys)
	      | T_OpenTuple tys => sel(i, tys)
	      | _ => err()
	    (* end case *)
	  end

  end
