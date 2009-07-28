(* effect.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Effect :> sig

    type effect

    val same    : (effect * effect) -> bool
    val compare : (effect * effect) -> order
    val hash    : effect -> word

    val toString : effect -> string

    val join : (effect * effect) -> effect

    structure Set : ORD_SET where type Key.ord_key = effect
    structure Map : ORD_MAP where type Key.ord_key = effect
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = effect

  end = struct

    structure S = ExnName.Set

    datatype effect
      = MightRaise of S.set

    fun exnSet (MightRaise fs) = fs

    fun same (f1, f2) = S.same (exnSet f1, exnSet f2)

    fun compare (f1, f2) = S.compare (exnSet f1, exnSet f2)

    fun hash f = let
      val sum = List.foldl Word.+ 0w0 
      in
       sum (List.map ExnName.hash (Set.listItems (exnSet f)))
      end

    fun toString (MightRaise exns) = let
      fun tos e = "  " ^ ExnName.toString e
      val es = Set.listItems exns
      in
        String.concat ["MightRaise{\n",
		       String.concatWith "\n" (List.map tos es),
		       "\n}"]
      end

    fun join (f1, f2) = MightRaise (S.union (exnSet f1, exnSet f2))

    structure Key = struct
      type ord_key = effect
      val compare = compare
    end

    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
      type hash_key = effect
      val hashVal = hash
      val sameKey = same
    end)

  end
