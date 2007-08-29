(* bom-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty
	
    datatype ty
      = T_Any				(* unknown type; uniform representation *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_Tuple of bool * ty list	(* heap-allocated tuple; the boolean is true for *)
					(* mutable tuples *)
      | T_Addr of ty			(* address of a tuple's field *)
      | T_Fun of (ty list * ty list * ty list)
					(* function type; the second argument is the type of *)
					(* the exception continuation(s) *)
      | T_Cont of ty list		(* first-class continuation *)
      | T_CFun of CFunctions.c_proto	(* C functions *)
      | T_VProc				(* address of VProc runtime structure *)
      | T_TyCon of tyc			(* high-level type constructor *)

    and tyc			      (* high-level type constructor *)
      = DataTyc of {
	  name : string,
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  nNullary : int,		(* the number of nullary constructors *)
	  cons : data_con list ref,	(* list of non-nullary constructors *)
	  rep : ty option ref		(* a cache of the representation type *)
	}
      | AbsTyc of {
	  name : string,
	  stamp : Stamp.stamp,
	  arity : int
	}

    and data_con = DCon of {	      (* a data-constructor function *)
	  name : string,		(* the name of the constructor *)
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  rep : dcon_rep,		(* the representation of values constructed by this *)
					(* constructor *)
	  argTy : ty list,		(* type(s) of argument(s) *)
	  myTyc : tyc
	}

    and dcon_rep		      (* representation of data-constructor functions; note: *)
				      (* this type does not include constants. *)
      = Transparent			(* for "CON of ty"; the data-constructor is represented *)
					(* directly by its argument *)
      | Tuple				(* for "CON of (ty * ... * ty)", where CON is the only *)
					(* constructor; represented as heap-allocated tuple of values *)
      | TaggedTuple of word		(* for when there are multiple constructors: the constructor *)
					(* is represented as heap-allocated tag/value pair *)

    val unitTy = T_Enum(0w0)
    val boolTy = T_Enum(0w1)	(* false = 0, true = 1 *)
    val exnTy = T_Any
    val exhTy = T_Cont[exnTy]
    val tidTy = T_Enum(0w0);
    val fiberTy = T_Cont[]

    val futureTyc = AbsTyc {name = "future", stamp = Stamp.new (), arity = 1}
    val futureTy = T_TyCon futureTyc

  (* compare types for equality *)
    fun equal (ty1, ty2) =
       case (ty1, ty2) of
          (T_Any, T_Any) => true
        | (T_Enum w1, T_Enum w2) => w1 = w2
        | (T_Raw rt1, T_Raw rt2) => rt1 = rt2
        | (T_Tuple (b1, tys1), T_Tuple (b2, tys2)) =>
             b1 = b2 andalso
             ListPair.allEq equal (tys1, tys2)
        | (T_Addr ty1, T_Addr ty2) => equal (ty1, ty2)
        | (T_Fun (argTys1, exhTys1, retTys1), 
           T_Fun (argTys2, exhTys2, retTys2)) =>
             ListPair.allEq equal (argTys1, argTys2) andalso
             ListPair.allEq equal (exhTys1, exhTys2) andalso
             ListPair.allEq equal (retTys1, retTys2)
        | (T_Cont argTys1, T_Cont argTys2) =>
             ListPair.allEq equal (argTys1, argTys2)
        | (T_CFun c_proto1, T_CFun c_proto2) =>
             c_proto1 = c_proto2
        | (T_TyCon tyc1, T_TyCon tyc2) => tyc_equal (tyc1, tyc2)
        | _ => false 
    and tyc_equal (tyc1, tyc2) =
       case (tyc1, tyc2) of
          (DataTyc {stamp = stamp1, ...}, 
           DataTyc {stamp = stamp2, ...}) =>
             Stamp.same (stamp1, stamp2)
        | (AbsTyc {stamp = stamp1, ...},
           AbsTyc {stamp = stamp2, ...}) =>
             Stamp.same (stamp1, stamp2)
        | _ => false

  (* is a cast from the first type to the second type valid? *)
    fun validCast (ty1, ty2) = (case (ty1, ty2)
	   of (T_Addr ty1, T_Addr ty2) => equal(ty1, ty2)
	    | (T_Addr _, _) => false
	    | (_, T_Addr _) => false
	    | (T_Raw rty1, T_Raw rty2) => (rty1 = rty2)
	    | (T_Raw _, _) => false
	    | (_, T_Raw _) => false
	    | (T_Any, _) => true
	    | (_, T_Any) => true
	    | _ => equal(ty1, ty2)
	  (* end case *))

  (* does the first type "match" the second type (i.e., can its values be used
   * wherever the second type is expected?
   *)
    fun match (ty1, ty2) = (case (ty1, ty2)
	   of (T_Addr ty1, T_Addr ty2) => equal(ty1, ty2)
	    | (T_Addr _, _) => false
	    | (_, T_Addr _) => false
	    | (T_Raw rty1, T_Raw rty2) => (rty1 = rty2)
	    | (T_Raw _, _) => false
	    | (_, T_Raw _) => false
	    | (_, T_Any) => true
	    | (T_Fun(argTys1, exhTys1, retTys1), T_Fun(argTys2, exhTys2, retTys2)) =>
	      (* NOTE contravariance! *)
		ListPair.allEq match (argTys2, argTys1)
                andalso ListPair.allEq match (exhTys2, exhTys1)
		andalso ListPair.allEq match (retTys2, retTys1)
	    | _ => equal(ty1, ty2)
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
	      | T_Enum w => concat["enum(", Word.fmt StringCvt.DEC w, ")"]
	      | T_Raw ty => RawTypes.toString ty
	      | T_Tuple(false, tys) => concat("[" :: tys2l(tys, ["]"]))
	      | T_Tuple(true, tys) => concat("![" :: tys2l(tys, ["]"]))
	      | T_Addr ty => concat["addr(", toString ty, ")"]
	      | T_Fun(paramTys, exhTys, retTys) => let
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
	      | T_Cont tys => concat("cont(" :: tys2l(tys, [")"]))
	      | T_CFun cp => CFunctions.protoToString cp
	      | T_VProc=> "vproc"
	      | T_TyCon(DataTyc{name, ...}) => name
	      | T_TyCon(AbsTyc{name, ...}) => name
	    (* end case *)
	  end

  (* wrapped raw values are stored in tuples *)
    fun wrap (ty as T_Raw _) = T_Tuple(false, [ty])
      | wrap ty = raise Fail(concat["wrap(", toString ty, ")"])

    fun unwrap (T_Tuple(false, [ty as T_Raw _])) = ty
      | unwrap ty = raise Fail(concat["unwrap(", toString ty, ")"])

  (* view a type as a function type *)
    fun asFunTy (T_Fun arg) = arg
      | asFunTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* get the return type(s) of a function type *)
    fun returnTy (T_Fun(_, _, ty)) = ty
      | returnTy (T_Cont _) = []
      | returnTy ty = raise Fail("expected function type, but found " ^ toString ty)

  (* view as tycon *)
    fun asTyc (T_TyCon tyc) = tyc
      | asTyc ty = raise Fail("expected tyc, but found " ^ toString ty)

  end
