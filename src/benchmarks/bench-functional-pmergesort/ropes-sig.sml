(* ropes-sig.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ropes in SML.
 * N.B. Not directly used in the compiler.
 *)

signature ROPES = sig

  type 'a rope

  val maxLeafSize : int
  val empty       : 'a rope
  val isEmpty     : 'a rope -> bool
  val isLeaf      : 'a rope -> bool
  val length      : 'a rope -> int
  val depth       : 'a rope -> int
  val concat      : 'a rope * 'a rope -> 'a rope
  val balance     : 'a rope -> 'a rope
  val sub         : 'a rope * int -> 'a
  val splitAt     : 'a rope * int -> ('a rope * 'a rope)

  val toList      : 'a rope -> 'a list
  val fromList    : 'a list -> 'a rope

end
