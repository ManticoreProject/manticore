(* flatten-rep.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for dealing with the flattened representation of data-constructor arguments.
 *)

structure FlattenRep : sig

  (* the flatten and unflatten functions are used to generate glue code that maps between
   * the AST constructor arguments and the (possibly flattened) BOM constructor arguments.
   * Each takes a list of AST data-constructor arguments and returns a list of BOM
   * data-constructor arguments and a list of bindings that define the AST argument
   * variables.
   *)
    type flatten_fn = BOM.var list -> ((BOM.var list * BOM.rhs) list * BOM.var list)
    type unflatten_fn = BOM.var list -> (BOM.var list * (BOM.var list * BOM.rhs) list)

    val flattenId : flatten_fn
    val unflattenId : unflatten_fn

    val flatten : BOM.ty -> (flatten_fn * unflatten_fn * BOM.ty list)

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy

    type flatten_fn = B.var list -> ((B.var list * B.rhs) list * B.var list)
    type unflatten_fn = B.var list -> (B.var list * (B.var list * B.rhs) list)

    val flattenId : flatten_fn = (fn xs => ([], xs))
    val unflattenId : unflatten_fn = (fn xs => (xs, []))

    datatype rep_tree
      = ATOM of BTy.ty
      | TUPLE of (BTy.ty list * rep_tree list)

    fun mkTmp ty = BV.new("_t", ty)

  (* compute the representation tree for a BOM type *)
    fun repTreeOf (BTy.T_Tuple(false, [ty as BTy.T_Raw _])) = ATOM ty
      | repTreeOf (BTy.T_Tuple(false, tys)) = TUPLE(tys, List.map repTreeOf tys)
      | repTreeOf ty = ATOM ty

  (* return the flat list of types for a representation tree *)
    fun repToTys (ATOM ty) = [ty]
      | repToTys (TUPLE(_, reps)) = List.foldr (fn (rep, tys) => repToTys rep @ tys) [] reps

  (* return the flatten function for a representation tree *)
    fun flattenRep (ATOM _) = flattenId
      | flattenRep (TUPLE(_, reps)) = let
	  fun sel (arg, _, []) = []
	    | sel (arg, i, x::xs) = ([x], B.E_Select(i, arg)) :: sel(arg, i+1, xs)
	  fun flat ([], []) = ([], [])
	    | flat (x::xs, rep::reps) = let
		val (binds, ys) = flat (xs, reps)
		in
		  case rep
		   of ATOM _ => (binds, x::ys)
		    | TUPLE(tys, reps) => let
			val args = List.map mkTmp tys
			val binds' = sel(x, 0, args)
			val (binds'', zs) = flat (args, reps)
			in
			  (binds' @ binds'' @ binds, zs@ys)
			end
		  (* end case *)
		end
	    | flat _ = raise Fail "arity mismatch"
	  in
	    fn xs => flat(xs, reps)
	  end

  (* return the unflatten function for a representation tree *)
    fun unflattenRep (ATOM _) = unflattenId
      | unflattenRep (TUPLE(_, reps)) = let
	  fun unflat ([], []) = ([], [])
	    | unflat (x::xs, rep::reps) = let
		val (ys, binds) = unflat (xs, reps)
		in
		  case rep
		   of ATOM _ => (x::ys, binds)
		    | TUPLE(tys, reps) => let
			val args = List.map mkTmp tys
			val (zs, binds') = unflat(args, reps)
			val alloc = ([x], B.E_Alloc(BTy.T_Tuple(false, tys), args))
			in
			  (zs@ys, binds' @ (alloc :: binds))
			end
		  (* end case *)
		end
	    | unflat _ = raise Fail "arity mismatch"
	  in
	    fn xs => unflat(xs, reps)
	  end

    fun flatten ty = let
	  val rep = repTreeOf ty
	  in
	    (flattenRep rep, unflattenRep rep, repToTys rep)
	  end

  end
