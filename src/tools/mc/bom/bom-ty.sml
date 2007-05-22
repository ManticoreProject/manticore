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
      | T_Wrap of raw_ty		(* boxed raw value *)
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
    val exhTy = T_Cont[T_Any]
    val tidTy = T_Any

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
	      | T_Tuple(false, tys) => concat("(" :: tys2l(tys, [")"]))
	      | T_Tuple(true, tys) => concat("!(" :: tys2l(tys, [")"]))
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
    fun asTyc (T_TyCon tyc) = tyc
      | asTyc ty = raise Fail("expected tyc, but found " ^ toString ty)

  end
