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

    fun same (f1, f2) = S.equal (exnSet f1, exnSet f2)

    fun compare (f1, f2) = S.compare (exnSet f1, exnSet f2)

  (* n.b. This hashing algorithm for collections is from 
   * Joshua Bloch's _Effective Java_, pp. 38-39
   *)
    fun hash f = let
      val initVal = 0w17
      fun lp ([], acc) = acc
	| lp (exn::t, acc) = let
            val h = ExnName.hash exn
            in
              lp (t, 0w37 * acc + h)
            end
      in
        lp (S.listItems (exnSet f), initVal)
      end

    fun toString (MightRaise exns) = let
      val es = S.listItems exns
      val ss = String.concatWith "\n  " (List.map ExnName.toString es)
      in
        String.concat ["MightRaise{\n", ss, "\n}"]
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
