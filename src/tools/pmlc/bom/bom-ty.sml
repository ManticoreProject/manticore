(* bom-ty.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTy =
  struct

  (* kinds for BOM types *)
    datatype kind
      = K_RAW		(* raw bits *)
      | K_BOXED		(* heap pointer *)
      | K_UNBOXED	(* tagged integer *)
      | K_UNIFORM	(* either K_BOXED or K_UNBOXED *)
      | K_TYPE		(* type (any of the above kinds) *)

    datatype raw_ty = datatype RawTypes.raw_ty

    datatype ty
      = T_Con of tyc * ty list
      | T_Record of (bool * ty) list
      | T_Cont of ty list
      | T_Fun of (ty list * ty list * ty list)
      | T_Raw of raw_ty			(* raw machine type *)
      | T_VProc				(* address of VProc runtime structure *)
      | T_Addr of ty			(* address of a tuple's field *)
      | T_Any				(* unknown type; uniform representation *)
      | T_CFun of CFunctions.c_proto	(* C functions *)
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)

    and tyc			      (* high-level type constructor *)
      = DataTyc of {
	  name : string,
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  nNullary : int,		(* the number of nullary constructors *)
	  cons : data_con list ref,	(* list of constructors *)
	  rep : ty ref,			(* type of the representation *)
	  kind : kind ref		(* kind of the representation: either UNBOXED, BOXED, *)
					(* or UNIFORM *)
	}

    and data_con = DCon of {	      (* a data-constructor function *)
	  name : string,		(* the name of the constructor *)
	  stamp : Stamp.stamp,		(* a unique stamp *)
	  rep : dcon_rep,		(* the representation of values constructed by this *)
					(* constructor *)
	  argTy : ty list,		(* type(s) of argument(s) to this constructor *)
	  myTyc : tyc			(* the datatype that this constructor belongs to *)
	}

    and dcon_rep		      (* representation of data-constructor functions; note: *)
				      (* this type does not include constants. *)
      = Enum of word			(* nullary constructor *)
      | Transparent			(* for "CON of ty"; the data-constructor is represented *)
					(* directly by its argument *)
      | Tuple				(* for "CON of (ty * ... * ty)", where CON is the only *)
					(* constructor; represented as heap-allocated tuple of values *)
      | TaggedTuple of word		(* for when there are multiple constructors: the constructor *)
					(* is represented as heap-allocated tag/value pair *)
      | ExnRep				(* exception constructors *)

    val unitTy = T_Enum 0w0

    val exnTyc = AbsTyc{
	    name = "exn",
	    stamp = Stamp.new(),
	    arity = 0
	  }
    val exnTy = T_TyCon exnTyc
    val exhTy = T_Cont[exnTy]

    val tidTy = T_Any
    val fiberTy = T_Cont[unitTy]
    val workQueueTy = T_Any

  (* construct an immutable tuple type; nullary tuples are unit type and singleton
   * tuples have a direct representation.
   *)
    fun tupleTy [] = unitTy
      | tupleTy [ty] = ty
      | tupleTy tys = T_Tuple(false, tys)

    val thunkTy = T_Fun([unitTy], [exhTy], [T_Any])
    val futureTy = T_Tuple(true, [T_Any, thunkTy, T_Any, T_Any])

  (* standard function types tuple their arguments and results *)
    fun stdFunTy (argTy, resTy) = T_Fun([tupleTy argTy], [exhTy], [tupleTy resTy])

  end
