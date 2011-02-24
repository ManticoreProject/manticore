(* ft-data-con.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure FTDataCon : sig

  (* create a new data constructor and add it to the list of constructors in its parent. *)
    val new : FTReprTypes.tycon -> (Atom.atom * FTReprTypes.ty option) -> FTReprTypes.dcon

  (* return true if two data constructors are the same *)
    val same : FTReprTypes.dcon * FTReprTypes.dcon -> bool

  (* compare two data constructors *)
    val compare : FTReprTypes.dcon * FTReprTypes.dcon -> order

  (* return the name of the data constructor *)
    val nameOf : FTReprTypes.dcon -> string

  (* return the ID of the data constructor *)
    val idOf : FTReprTypes.dcon -> int

  (* return the type of the data constructor; for data constants, this type
   * will be a ConTy, while for data constructors it will be a FunTy.
   *)
    val typeOf : FTReprTypes.dcon -> FLAST.ty_scheme

  (* return the argument type of the data constructor (if any) *)
    val argTypeOf : FTReprTypes.dcon -> FTReprTypes.ty option

  (* return the instantiated type/argument type of the data constructor *)
    val typeOf' : FTReprTypes.dcon * FTReprTypes.ty list -> FTReprTypes.ty
    val argTypeOf' : FTReprTypes.dcon * FTReprTypes.ty list -> FTReprTypes.ty option
    val resultTypeOf' : FTReprTypes.dcon * FTReprTypes.ty list -> FTReprTypes.ty

  (* return the datatype type constructor that owns this data constructor *)
    val ownerOf : FTReprTypes.dcon -> FTReprTypes.tycon

  (* return true if the data constructor is nullary *)
    val isNullary : FTReprTypes.dcon -> bool

    val toString : FTReprTypes.dcon -> string

  (* hash tables keyed by data constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = FTReprTypes.dcon

   end = struct

    structure R = FTReprTypes

    datatype dcon = datatype R.dcon

    fun new (tyc as R.Tyc {def=R.DataTyc{nCons, cons}, ...}) = let
	  fun add (name, argTy) = let
		val id = !nCons
		val dcon = DCon{id = id, name = name, owner = tyc, argTy = argTy}
		in
		  nCons := id + 1;
		  cons := !cons @ [dcon];
		  dcon
		end
	  in
	    add
	  end
      | new _ = raise Fail "AbsTyc"

    fun same (DCon{owner=o1, id=a, ...}, DCon{owner=o2, id=b, ...}) =
	  (a = b) andalso FTTyCon.same(o1, o2)

    fun hash (DCon{owner=R.Tyc{stamp, ...}, id, ...}) =
	  (0w7 * Stamp.hash stamp) + Word.fromInt id

    fun compare (DCon{owner=o1, id=a, ...}, DCon{owner=o2, id=b, ...}) = (
	  case Int.compare(a, b)
	   of EQUAL => FTTyCon.compare(o1, o2)
	    | order => order
	  (* end case *))

    fun nameOf (DCon{name, ...}) = Atom.toString name

    fun argTypeOf (DCon{argTy, ...}) = argTy

    fun ownerOf (DCon{owner, ...}) = owner

    fun typeOf (DCon{owner as R.Tyc{params, ...}, argTy, ...}) = let
	  val ty = R.ConTy(List.map R.VarTy params, owner)
	  in
	    case argTy
	     of NONE => FLAST.TyScheme(params, ty)
	      | SOME ty' => FLAST.TyScheme(params, R.FunTy(ty', ty))
	    (* end case *)
	  end

    fun typeOf' (dc, args) = FTTypeUtil.apply (typeOf dc, args)

    fun resultTypeOf' (DCon{owner as R.Tyc{params, ...}, ...}, args) = let
	  val ty = R.ConTy (List.map R.VarTy params, owner)
	  in
	    FTTypeUtil.apply (FLAST.TyScheme(params, ty), args)
	  end

    fun argTypeOf' (DCon {owner as R.Tyc {params, ...}, argTy, ...}, args) = (
	  case argTy
	   of NONE => NONE
	    | SOME ty => SOME (FTTypeUtil.apply (FLAST.TyScheme(params, ty), args))
	  (* end case *))

    fun idOf (DCon{id, ...}) = id

    fun isNullary (DCon{argTy = NONE, ...}) = true
      | isNullary _ = false

    fun toString (DCon {id, name, owner, argTy}) = 
      String.concat [Atom.toString name, 
		     "(", Option.getOpt (Option.map R.toString argTy, ""), ")",
		     "[", Int.toString id, "]",
		     ":", FTTyCon.toString owner]

    structure Tbl = HashTableFn (
      struct
	type hash_key = dcon
	val hashVal = hash
	val sameKey = same
      end)

  end
