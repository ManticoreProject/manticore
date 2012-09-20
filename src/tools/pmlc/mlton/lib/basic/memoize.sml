(* Copyright (C) 2012 John Reppy
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *
 * This module contains a version of the "memoize" function from the MLton
 * compiler's String module.
 *)

structure Memoize : sig

    val memoize : (string -> 'a) -> string -> 'a
    val memoizeList : (string -> 'a) * (string * 'a) list -> string -> 'a

  end = struct

    fun hash s = CharVector.foldl (fn (c, h) => Word.fromInt(Char.ord c) + Word.* (h, 0w31)) 0w0 s

    fun memoizeList (init : string -> 'a, l : (string * 'a) list) : string -> 'a = let
	  val set: (word * string * 'a) HashSet.t = HashSet.new {hash = #1}
	  fun lookupOrInsert (s, f) = let
		val hash = hash s
		in HashSet.lookupOrInsert
		   (set, hash,
		    fn (hash', s', _) => hash = hash' andalso s = s',
		    fn () => (hash, s, f ()))
		end
	  val _ = MLtonList.foreach (l, fn (s, a) => ignore (lookupOrInsert (s, fn () => a)))
	  in
	    fn s => #3 (lookupOrInsert (s, fn () => init s))
	  end

    fun memoize init = memoizeList (init, [])

  end
