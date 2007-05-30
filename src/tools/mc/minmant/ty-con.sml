(* ty-con.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *
 * Utility operations on type constructors.
 *)

structure TyCon : sig

  (* create a new abstract type constructor *)
    val newAbsTyc : (Atom.atom * int * bool) -> Types.tycon

  (* create a new datatype tyc; it will have an empty constructor list *)
    val newDataTyc : (Atom.atom * AST.tyvar list) -> Types.tycon

  (* return the name of a type constructor *)
    val nameOf : Types.tycon -> Atom.atom

  (* return true if two type constructors are the same *)
    val same : Types.tycon * Types.tycon -> bool

  (* return the arity of a type constructor *)
    val arityOf : Types.tycon -> int

  (* hash tables keyed by type constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = Types.tycon

  end = struct

    datatype tycon = datatype Types.tycon

  (* create a new abstract type constructor *)
    fun newAbsTyc (name, arity, eq) = AbsTyc{
	    name = name,
	    stamp = Stamp.new(),
	    arity = arity,
	    eq = eq
	  }

  (* create a new datatype tyc; it will have an empty constructor list *)
    fun newDataTyc (name, params) = DataTyc{
	    name = name,
	    stamp = Stamp.new(),
	    params = params,
	    cons = ref[]
	  }

  (* return the name of a type constructor *)
    fun nameOf (AbsTyc{name, ...}) = name
      | nameOf (DataTyc{name, ...}) = name

    fun stampOf (AbsTyc{stamp, ...}) = stamp
      | stampOf (DataTyc{stamp, ...}) = stamp

  (* return true if two type constructors are the same *)
    fun same (tyc1, tyc2) = Stamp.same(stampOf tyc1, stampOf tyc2)

  (* return the arity of a type constructor *)
    fun arityOf (AbsTyc{arity, ...}) = arity
      | arityOf (DataTyc{params, ...}) = List.length params

    structure Tbl = HashTableFn (
      struct
	type hash_key = tycon
	fun hashVal (AbsTyc{stamp, ...}) = Stamp.hash stamp
	  | hashVal (DataTyc{stamp, ...}) = Stamp.hash stamp
	val sameKey = same
      end)

  end
