(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RANDOM =
   sig
      val alphaNumString: int -> string
      val charFrom: string -> char
      val bool: unit -> bool
      val list: 'a list -> 'a option
      (* natLessThan n returns a random number uniform in [0, n). *)
      val natLessThan: int -> int
      val nRandom: {list: 'a list, length: int, n: int} -> 'a list
      (* 0.0 <= real() <= 1.0 *)
      val real: unit -> real
      val seed: unit -> (*Word.t*)word option
      val srand: (*Word.t*)word -> unit
      val useed: unit -> (*Word.t*)word option
      val word: unit -> (*Word.t*)word
      (* word w returns a random number uniform in [0, w). *)
      val wordLessThan: (*Word.t*)word -> (*Word.t*)word
   end
