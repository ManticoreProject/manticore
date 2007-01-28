(* stamp.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Stamps are locally unique identifiers used in the compiler to
 * distinguish different types, variables, etc.  For a given compilation,
 * the stamp assigned to an object is guaranteed to be unique, although
 * an object may have different stamps assigned to it in different compiles.
 *)

structure Stamp :> sig

    type stamp

    val new : unit -> stamp

    val same : (stamp * stamp) -> bool
    val compare : (stamp * stamp) -> order
    val hash : stamp -> word

    val toString : stamp -> string

    structure Set : ORD_SET where type Key.ord_key = stamp
    structure Map : ORD_MAP where type Key.ord_key = stamp
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = stamp

  (* a reserved stamp used for things that need stamps, but there is not
   * a natural source of the stamp.
   *)
    val builtin : stamp

  end = struct

    structure W = Word

    datatype stamp = STAMP of {
	id : Word.word
      }

    val cnt = ref 0w0

    fun new () = let val w = !cnt in cnt := w+0w1; STAMP{id = w} end

    fun same (STAMP{id, ...}, STAMP{id=id', ...}) = (id = id')
    fun compare (STAMP{id, ...}, STAMP{id=id', ...}) = W.compare(id, id')
    fun hash (STAMP{id, ...}) = id

    fun toString (STAMP{id, ...}) =
	  concat["<", StringCvt.padLeft #"0" 4 (W.toString id), ">"]

    structure Key =
      struct
	type ord_key = stamp
	val compare = compare
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
	type hash_key = stamp
	fun hashVal (STAMP{id}) = id
	fun sameKey (STAMP{id=a}, STAMP{id=b}) = (a = b)
      end)

    val builtin = new()

  end

