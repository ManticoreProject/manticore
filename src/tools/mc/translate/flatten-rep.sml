(* flatten-rep.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for dealing with the flattened representation of data-constructor arguments.
 *)

structure FlattenRep : sig

  (* an unflatten function takes a list of AST data-constructor arguments and returns a list
   * of BOM data-constructor arguments and a lists of bindings that define the AST argument
   * variables.
   *)
    type unflatten_fn = BOM.var list -> (BOM.var list * (BOM.var list * BOM.rhs) list)

    val flatten : BOM.ty -> (unflatten_fn * BOM.ty list)

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy

    type unflatten_fn = B.var list -> (B.var list * (B.var list * B.rhs) list)

    datatype rep_tree
      = ATOM of BTy.ty
      | TUPLE of (BTy.ty list * rep_tree list)

    fun mkTmp ty = BV.new("_t", ty)

  (* compute the representation tree for a BOM type *)
    fun repTreeOf (BTy.T_Tuple(false, [ty as BTy.T_Raw _])) = ATOM ty
      | repTreeOf (BTy.T_Tuple(false, tys)) = TUPLE(tys, List.map repTreeOf tys)
      | repTreeOf ty = ATOM ty

  (* return the flat list of types for a representation tree *)
    fun flattenRep (ATOM ty) = [ty]
      | flattenRep (TUPLE(_, reps)) = List.foldr (fn (rep, tys) => flattenRep rep @ tys) [] reps

  (* return the unflatten function for a representation tree *)
    fun unflattenRep (ATOM _) = (fn xs => (xs, []))
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
	    (unflattenRep rep, flattenRep rep)
	  end

  end
