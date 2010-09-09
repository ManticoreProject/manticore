(* prog-point.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Program point labels.  These are represented as property list holders.
 * We assign unique IDs (stored as a property) on demand.
 *)

structure ProgPt :> sig

    type ppt

    val new : unit -> ppt

    val compare : (ppt * ppt) -> order
    val hash : ppt -> word
    val same : (ppt * ppt) -> bool

    val toString : ppt -> string
    val label : ppt -> string option

    val newProp : (ppt -> 'a) -> {
	    clrFn  : ppt -> unit,
	    getFn  : ppt -> 'a,
	    setFn : (ppt * 'a) -> unit,
	    peekFn : ppt -> 'a option
	  }

    val newFlag : unit -> {
	    getFn : ppt -> bool,
	    setFn : (ppt * bool) -> unit
	  }

    structure Set : ORD_SET where type Key.ord_key = ppt
    structure Map : ORD_MAP where type Key.ord_key = ppt
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = ppt

  end = struct

    type ppt = PropList.holder

    val count = ref 0w0

    fun newId () = let val n = !count in count := n+0w1; n end

    val {getFn=getId, ...} = PropList.newProp (fn h => h, fn _ => newId())

    fun new () = PropList.newHolder()

    fun compare (a, b)= Word.compare(getId a, getId b)
    val hash = getId
    val same = PropList.sameHolder

    fun toString (pt : ppt) = Format.format "P%04x" [Format.WORD(getId pt)]
    fun label pt = if PropList.hasProps pt then SOME(toString pt) else NONE

    fun newProp f = PropList.newProp (fn h => h, f)
    fun newFlag () = PropList.newFlag (fn h => h)

    structure K = struct
	type ord_key = ppt
	val compare = compare
      end

    structure Set = RedBlackSetFn (K)
    structure Map = RedBlackMapFn (K)
    structure Tbl = HashTableFn (
      struct
	type hash_key = ppt
	val hashVal = hash
	val sameKey = same
      end)

  end
