(* ranges.sig
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ranges in SML.
 * N.B. Not directly used in the compiler.
 *)

signature RANGES = sig

  type range

  structure Ropes : ROPES
  val mkRange  : int * int * int -> range
  val toRope   : range -> int Ropes.rope
  val toString : range -> string

  (* FIXME this is here for type reasons *)
  val ropeToString : int Ropes.rope -> string

end
