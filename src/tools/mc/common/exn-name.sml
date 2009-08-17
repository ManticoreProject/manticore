(* exn-name.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Exception names.
 *)

structure ExnName :> sig

    type exn_name

    val new : string -> exn_name

    val same    : (exn_name * exn_name) -> bool
    val compare : (exn_name * exn_name) -> order
    val hash    : exn_name -> word

    val toString : exn_name -> string

    structure Set : ORD_SET where type Key.ord_key = exn_name
    structure Map : ORD_MAP where type Key.ord_key = exn_name
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = exn_name

  end = struct

    datatype exn_name = ExnName of {
        name  : string,
	stamp : Stamp.stamp
      }

    fun new e = let
      val s = Stamp.new ()
      in  
        ExnName {name=e, stamp=s}
      end

    fun stampOf (ExnName {stamp=s, ...}) = s

    fun >> f (x, y) = (f x, f y)

    val same    = Stamp.same o (>> stampOf)
    val compare = Stamp.compare o (>> stampOf)
    val hash    = Stamp.hash o stampOf

    fun toString (ExnName {name=e, stamp=s}) = e ^ Stamp.toString s

    structure Key =
      struct
	type ord_key = exn_name
	val compare = compare
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
	type hash_key = exn_name
        val hashVal = hash
	val sameKey = same
      end)

  end
