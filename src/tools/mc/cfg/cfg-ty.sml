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
      | T_KnownFunc of ty list
      | T_Block of ty list

    val unitTy = T_Enum(0w0)
    val boolTy = T_Enum(0w1)	(* false = 0, true = 1 *)

    fun equal (ty1, ty2) = (case (ty1, ty2)
	   of (T_Any, T_Any) => true
            | (T_Enum w1, T_Enum w2) => (w1 = w2)
            | (T_Raw rty1, T_Raw rty2) => (rty1 = rty2)
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
            | (T_KnownFunc ty1s, T_KnownFunc ty2s) => equalList (ty1s, ty2s)
            | (T_Block ty1s, T_Block ty2s) => equalList (ty1s, ty2s)
            | _ => false
	  (* end case *))

    and equalList (tys1, tys2) = ListPair.allEq equal (tys1, tys2)
  (* return true if the type has a single-word uniform representation; this includes
   * anything represented by a pointer or a tagged integer.
   *)
    fun hasUniformRep ty = (case ty 
           of T_Raw _ => false
	    | T_VProc => false
            | _ => true
	  (* end case *))

  (* return true if the type is represented by a pointer (including pointers
   * that lie outside the heap).
   *)
    fun isBoxed ty = (case ty 
           of T_Any => false
            | T_Enum _ => false
            | T_Raw _ => false
            | T_Tuple _ => true
            | T_OpenTuple _ => true
	    | T_Addr _ => raise Fail "isBoxed(addr)"
	    | T_CFun _ => true
	    | T_VProc => true
            | T_StdFun _ => true
            | T_StdCont _ => true
            | T_KnownFunc _ => true
            | T_Block _ => true
	  (* end case *))

  (* is fromTy a more specific instance of toTy? *)
    fun match (fromTy, toTy) = (case (fromTy, toTy)
           of (T_Any, T_KnownFunc _) => false
            | (T_Any, T_Block _) => false
	    | (T_KnownFunc _, T_Any) => false
	    | (T_Block _, T_Any) => false
	    | (T_Any, T_Addr _) => false
	    | (T_Addr _, T_Any) => false
            | (fromTy, T_Any) => hasUniformRep fromTy
	  (* the following shouldn't be here, since it isn't really sound, but we need it
	   * to handle surface-language polymorphism, which is translated to T_Any.
	   *)
	    | (T_Any, toTy) => hasUniformRep toTy
            | (T_OpenTuple ty1s, T_OpenTuple ty2s) => let
		fun ok (_, []) = true
		  | ok (ty1::r1, ty2::r2) = match(ty1, ty2) andalso ok(r1, r2)
		  | ok ([], _) = false
		in
		  ok (ty1s, ty2s)
		end
	    | (T_Tuple(false, tys1), T_OpenTuple tys2) => let
		fun ok (_, []) = true
		  | ok (ty1::r1, ty2::r2) = match(ty1, ty2) andalso ok(r1, r2)
		  | ok ([], _) = false
		in
		  ok (tys1, tys2)
		end
            | (T_Tuple(mut1, ty1s), T_Tuple(mut2, ty2s)) =>
		(mut1 = mut2) andalso ListPair.allEq match (ty1s, ty2s)
            | (T_StdFun{clos = clos1, args = args1, ret = ret1, exh = exh1},
               T_StdFun{clos = clos2, args = args2, ret = ret2, exh = exh2}) =>
              (* Note contravariance for arguments! *)
                  (match (clos2, clos1) orelse isValidCast (clos2, clos1)) andalso
                  ListPair.allEq match (args2, args1) andalso
                  match (ret2, ret1) andalso
                  match (exh2, exh1)
            | (T_StdCont{clos = clos1, args = args1}, 
               T_StdCont{clos = clos2, args = args2}) =>
              (* Note contravariance for arguments! *)
                  (match (clos2, clos1) orelse isValidCast (clos2, clos1)) andalso
                  ListPair.allEq match (args2, args1)
            | (T_KnownFunc ty1s, T_KnownFunc ty2s) => ListPair.allEq match (ty2s, ty1s)
            | (T_Block ty1s, T_Block ty2s) => ListPair.allEq match (ty2s, ty1s)
            | _ => equal (fromTy, toTy)
	  (* end case *))

  (* is it legal to cast from fromTo to toTy? *)
    and isValidCast (fromTy, toTy) = (case (fromTy, toTy)
           of (T_Any, T_KnownFunc _) => false
            | (T_Any, T_Block _) => false
	    | (T_KnownFunc _, T_Any) => false
	    | (T_Block _, T_Any) => false
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
	      | T_KnownFunc tys => concat("kfnc(" :: tys2l(tys, [")"]))
	      | T_Block tys => concat("blck(" :: tys2l(tys, [")"]))
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
