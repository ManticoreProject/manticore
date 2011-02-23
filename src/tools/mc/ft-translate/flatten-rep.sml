(* flatten-rep.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for dealing with the flattened representation of data-constructor arguments.
 *)

structure FlattenRep : sig

    datatype rep_tree
      = ATOM of BOM.ty
      | TUPLE of (BOM.ty list * rep_tree list)

  (* given the BOM type of an AST data-constructor argument type, return the representation
   * tree that guides how the argument type is to be represented in BOM.
   *)
    val flattenRep : BOM.ty -> rep_tree

    val srcTys : rep_tree -> BOM.ty list	(* BOM types of AST argument(s) *)
    val dstTys : rep_tree -> BOM.ty list	(* BOM types of BOM argument(s) *)

  (* the flatten and unflatten functions are used to generate glue code that maps between
   * the AST constructor arguments and the (possibly flattened) BOM constructor arguments.
   * Each takes a list of AST data-constructor arguments and returns a list of BOM
   * data-constructor arguments and a list of bindings that define the AST argument
   * variables.
   *)
    val flatten : (rep_tree * BOM.var list) -> ((BOM.var list * BOM.rhs) list * BOM.var list)
    val unflatten : (rep_tree * BOM.var list) -> (BOM.var list * (BOM.var list * BOM.rhs) list)

  (* for debugging *)
    val fmt : {long : bool} -> rep_tree -> string
    val toString : rep_tree -> string

  end = struct

    structure B = BOM
    structure BV = B.Var
    structure BTy = BOMTy

    datatype rep_tree
      = ATOM of BOM.ty
      | TUPLE of (BOM.ty list * rep_tree list)

  (* for debugging *)
    fun fmt {long} = let
	  val ty2s = if long then BOMTyUtil.toString else fn _ => "*"
	  fun toS (ATOM ty, l) = ty2s ty :: l
	    | toS (TUPLE(_, reps), l) = let
		fun toS' ([], l) = l
		  | toS' ([rep], l) = toS(rep, l)
		  | toS' (rep::reps, l) = toS(rep, ", " :: toS'(reps, l))
		in
		  "[" :: toS'(reps, "]" :: l)
		end
	  in
	    fn rep => String.concat(toS(rep, []))
	  end

    val toString = fmt {long=false}

    fun mkTmp ty = BV.new("_t", ty)

  (* compute the representation tree for a BOM type that is the translation of the argument
   * type of a data constructor.
   *)
    fun flattenRep ty = let
	  fun repOf (BTy.T_Tuple(false, tys)) = TUPLE(tys, List.map repOf tys)
	    | repOf ty = ATOM ty
	  in
	  (* NOTE: we need to treat the case where the argument type is a singleton tuple
	   * type specially.  This case arises when the argument is a wrapped raw type (there
	   * are not singleton tuples in the AST) and we add an extra TUPLE wrapper to ensure
	   * that the correct code is produced by flatten/unflatten.  For example, the constructor
	   *
	   *    FOO of double
	   *
	   * will have the rep tree "[[double]]".
	   *)
	    case ty
	     of BTy.T_Tuple(false, [_]) => TUPLE([ty], [repOf ty])
	      | _ => repOf ty
	  end

  (* return the types of the AST arguments *)
    fun srcTys (ATOM ty) = [ty]
      | srcTys (TUPLE(tys, _)) = tys

  (* return the flat list of types for a representation tree, which is used as the
   * argument type for the BOM data constructor.
   *)
    fun dstTys (ATOM ty) = [ty]
      | dstTys (TUPLE(_, reps)) = List.foldr (fn (rep, tys) => dstTys rep @ tys) [] reps

  (* return the flatten function for a representation tree *)
    fun flatten (ATOM _, xs) = ([], xs)
      | flatten (rep as TUPLE(_, reps), xs) = let
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
	    | flat _ = raise Fail(concat[
		  "flatten(", fmt {long=true} rep, ", ", Int.toString(List.length xs),
		  "): arity mismatch"
		])
	  in
	    flat(xs, reps)
	  end

  (* return the unflatten function for a representation tree *)
    fun unflatten (ATOM _, xs) = (xs, [])
      | unflatten (rep as TUPLE(_, reps), xs) = let
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
	    | unflat _ = raise Fail(concat[
		  "unflatten(", fmt {long=true} rep, ", ", Int.toString(List.length xs),
		  "): arity mismatch"
		])
	  in
	    unflat(xs, reps)
	  end

  end
