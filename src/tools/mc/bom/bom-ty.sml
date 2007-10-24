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
	  rep : ty ref,			(* type of the representation *)
	  kind : kind ref		(* kind of the representation: either UNBOXED, BOXED, *)
					(* or UNIFORM *)
	}
(* QUESTION: are we using the following? *)
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
	  argTy : ty list,		(* type(s) of argument(s) to this constructor *)
	  myTyc : tyc			(* the datatype that this constructor belongs to *)
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
    val fiberTy = T_Cont[unitTy]
    val workQueueTy = T_Any

  (* construct an immutable tuple type; nullary tuples are unit type and singleton
   * tuples have a direct representation.
   *)
    fun tupleTy [] = unitTy
      | tupleTy [ty] = ty
      | tupleTy tys = T_Tuple(false, tys)

    val thunkTy = T_Fun([unitTy], [exhTy], [T_Any])
    val futureTy = T_Tuple(true, [T_Any, thunkTy])
    
  end
