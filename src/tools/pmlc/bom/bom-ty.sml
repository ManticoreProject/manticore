(* bom-ty.sml
 *
 * COPYRIGHT (c) 2014 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure BOMTy =
  struct

    datatype raw_ty = datatype RawTypes.raw_ty

  (* kinds for BOM types *)
    datatype kind
      = K_RAW		(* raw bits *)
      | K_BOXED		(* heap pointer *)
      | K_UNBOXED	(* tagged integer *)
      | K_UNIFORM	(* either K_BOXED or K_UNBOXED *)
      | K_TYPE		(* type (any of the above kinds) *)

    datatype ty_var = ??

    datatype raw_ty = datatype RawTypes.raw_ty

    datatype ty
      = T_Param of ty_var
      | T_Con of ty_con * ty list
      | T_Record of (int * bool * ty) list
      | T_Tuple of ty list
      | T_Fun of ty list * ty list * ty list
      | T_Cont of ty list
      | T_Array of ty
      | T_Vector of ty
      | T_Addr of ty
      | T_Exn
      | T_Bignum
      | T_Any
      | T_VProc
      | T_Raw of raw_ty
      | T_Enum of Word.word		(* unsigned tagged integer; word is max value <= 2^31-1 *)

(* QUESTION: should we fold Array, Vector, Addr, Bignum, etc into ty_con? *)

    and ty_con			      (* high-level type constructor *)
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
	  argTy : ty list,		(* type(s) of argument(s) to this constructor *)
	  myTyc : tyc			(* the datatype that this constructor belongs to *)
	}

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
      | tupleTy tys = T_Tuple tys

    fun thunkTy rngTy = T_Fun([unitTy], [exhTy], [rngTy])
(* FIXME
    val futureTy rngTy = T_Tuple(true, [T_Any, thunkTy rngTy, T_Any, T_Any])
*)

  (* standard function types tuple their arguments and results *)
    fun stdFunTy (argTy, resTy) = T_Fun([tupleTy argTy], [exhTy], [tupleTy resTy])

  end
