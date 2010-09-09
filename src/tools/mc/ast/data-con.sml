(* data-con.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure DataCon : sig

  (* create a new data constructor and add it to the list of constructors in its parent. *)
    val new : Types.tycon -> (Atom.atom * AST.ty option) -> AST.dcon

  (* return true if two data constructors are the same *)
    val same : AST.dcon * AST.dcon -> bool

  (* compare two data constructors *)
    val compare : AST.dcon * AST.dcon -> order

  (* return the name of the data constructor *)
    val nameOf : AST.dcon -> string

  (* return the ID of the data constructor *)
    val idOf : AST.dcon -> int

  (* return the type of the data constructor; for data constants, this type
   * will be a ConTy, while for data constructors it will be a FunTy.
   *)
    val typeOf : AST.dcon -> AST.ty_scheme

  (* return the argument type of the data constructor (if any) *)
    val argTypeOf : AST.dcon -> AST.ty option

  (* return the instantiated type/argument type of the data constructor *)
    val typeOf' : AST.dcon * AST.ty list -> AST.ty
    val argTypeOf' : AST.dcon * AST.ty list -> AST.ty option
    val resultTypeOf' : AST.dcon * AST.ty list -> AST.ty

  (* return the datatype type constructor that owns this data constructor *)
    val ownerOf : AST.dcon -> Types.tycon

  (* return true if the data constructor is nullary *)
    val isNullary : AST.dcon -> bool

    val toString : AST.dcon -> string

  (* hash tables keyed by data constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = AST.dcon

   end = struct

    datatype dcon = datatype AST.dcon

    fun new (tyc as Types.Tyc{def=Types.DataTyc{nCons, cons}, ...}) = let
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
	  (a = b) andalso TyCon.same(o1, o2)

    fun hash (DCon{owner=Types.Tyc{stamp, ...}, id, ...}) =
	  (0w7 * Stamp.hash stamp) + Word.fromInt id

    fun compare (DCon{owner=o1, id=a, ...}, DCon{owner=o2, id=b, ...}) = (
	  case Int.compare(a, b)
	   of EQUAL => TyCon.compare(o1, o2)
	    | order => order
	  (* end case *))

    fun nameOf (DCon{name, ...}) = Atom.toString name

    fun argTypeOf (DCon{argTy, ...}) = argTy

    fun ownerOf (DCon{owner, ...}) = owner

    fun typeOf (DCon{owner as Types.Tyc{params, ...}, argTy, ...}) = let
	  val ty = AST.ConTy(List.map AST.VarTy params, owner)
	  in
	    case argTy
	     of NONE => AST.TyScheme(params, ty)
	      | SOME ty' => AST.TyScheme(params, AST.FunTy(ty', ty))
	    (* end case *)
	  end

    fun typeOf' (dc, args) = TypeUtil.apply(typeOf dc, args)

    fun resultTypeOf' (DCon{owner as Types.Tyc{params, ...}, ...}, args) = let
	  val ty = AST.ConTy(List.map AST.VarTy params, owner)
	  in
	    TypeUtil.apply(AST.TyScheme(params, ty), args)
	  end

    fun argTypeOf' (DCon{owner as Types.Tyc{params, ...}, argTy, ...}, args) = (
	  case argTy
	   of NONE => NONE
	    | SOME ty => SOME(TypeUtil.apply(AST.TyScheme(params, ty), args))
	  (* end case *))

    fun idOf (DCon{id, ...}) = id

    fun isNullary (DCon{argTy = NONE, ...}) = true
      | isNullary _ = false

    fun toString (DCon {id, name, owner, argTy}) = 
      String.concat [Atom.toString name, 
		     "(", Option.getOpt (Option.map TypeUtil.toString argTy, ""), ")",
		     "[", Int.toString id, "]",
		     ":", TyCon.toString owner]

    structure Tbl = HashTableFn (
      struct
	type hash_key = dcon
	val hashVal = hash
	val sameKey = same
      end)

  end
