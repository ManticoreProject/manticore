(* ft-data-con.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure FTDataCon : sig

  (* create a new data constructor and add it to the list of constructors in its parent. *)
    val new : FTTypes.tycon -> (Atom.atom * FTTypes.ty option * Types.dcon) -> FTTypes.dcon

  (* return true if two data constructors are the same *)
    val same : FTTypes.dcon * FTTypes.dcon -> bool

  (* compare two data constructors *)
    val compare : FTTypes.dcon * FTTypes.dcon -> order

  (* return the name of the data constructor *)
    val nameOf : FTTypes.dcon -> string

  (* return the ID of the data constructor *)
    val idOf : FTTypes.dcon -> int

  (* return the type of the data constructor; for data constants, this type
   * will be a ConTy, while for data constructors it will be a FunTy.
   *)
    val typeOf : FTTypes.dcon -> FLAST.ty_scheme

  (* return the argument type of the data constructor (if any) *)
    val argTypeOf : FTTypes.dcon -> FTTypes.ty option

  (* return the instantiated type/argument type of the data constructor *)
    val typeOf' : FTTypes.dcon * FTTypes.ty list -> FTTypes.ty
    val argTypeOf' : FTTypes.dcon * FTTypes.ty list -> FTTypes.ty option
    val resultTypeOf' : FTTypes.dcon * FTTypes.ty list -> FTTypes.ty

  (* return the datatype type constructor that owns this data constructor *)
    val ownerOf : FTTypes.dcon -> FTTypes.tycon

  (* return the interface dcon corresponding to the argument dcon *)
  (* that is, go backwards to the AST dcon *)
  (* ex: interfaceOf FBasis.boolFalse --> Basis.boolFalse *)
    val interfaceOf : FTTypes.dcon -> Types.dcon

  (* return true if the data constructor is nullary *)
    val isNullary : FTTypes.dcon -> bool

    val toString : FTTypes.dcon -> string

  (* hash tables keyed by data constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = FTTypes.dcon

   end = struct

    structure T = FTTypes

    datatype dcon = datatype T.dcon

    fun new (tyc as T.Tyc {def=T.DataTyc{nCons, cons}, ...}) = let
	  fun add (name, argTy, interface) = let
		val id = !nCons
		val dcon = DCon{id = id, name = name, owner = tyc, argTy = argTy, 
				interface = interface}
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

    fun hash (DCon{owner=T.Tyc{stamp, ...}, id, ...}) =
	  (0w7 * Stamp.hash stamp) + Word.fromInt id

    fun compare (DCon{owner=o1, id=a, ...}, DCon{owner=o2, id=b, ...}) = (
	  case Int.compare(a, b)
	   of EQUAL => FTTyCon.compare(o1, o2)
	    | order => order
	  (* end case *))

    fun nameOf (DCon{name, ...}) = Atom.toString name

    fun argTypeOf (DCon{argTy, ...}) = argTy

    fun ownerOf (DCon{owner, ...}) = owner

    fun interfaceOf (DCon {interface, ...}) = interface

    fun typeOf (DCon{owner as T.Tyc{params, ...}, argTy, ...}) = let
	  val ty = T.ConTy (List.map T.VarTy params, owner)
	  in
	    case argTy
	     of NONE => FLAST.TyScheme(params, ty)
	      | SOME ty' => FLAST.TyScheme(params, T.FunTy(ty', ty))
	    (* end case *)
	  end

    fun typeOf' (dc, args) = FTTypeUtil.apply (typeOf dc, args)

    fun resultTypeOf' (DCon{owner as T.Tyc{params, ...}, ...}, args) = let
	  val ty = T.ConTy (List.map T.VarTy params, owner)
	  in
	    FTTypeUtil.apply (FLAST.TyScheme(params, ty), args)
	  end

    fun argTypeOf' (DCon {owner as T.Tyc {params, ...}, argTy, ...}, args) = (
	  case argTy
	   of NONE => NONE
	    | SOME ty => SOME (FTTypeUtil.apply (FLAST.TyScheme(params, ty), args))
	  (* end case *))

    fun interfaceDCon (DCon {interface=i, ...}) = i

    fun idOf (DCon{id, ...}) = id

    fun isNullary (DCon{argTy = NONE, ...}) = true
      | isNullary _ = false

    fun toString (DCon {id, name, owner, argTy, interface}) = 
      String.concat [Atom.toString name, 
		     "(", Option.getOpt (Option.map T.toString argTy, ""), ")",
		     "[", Int.toString id, "]",
		     ":", FTTyCon.toString owner]

    structure Tbl = HashTableFn (
      struct
	type hash_key = dcon
	val hashVal = hash
	val sameKey = same
      end)

  end
