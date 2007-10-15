(* ropes.sig
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
  val ropeLen     : 'a rope -> int
  val ropeDepth   : 'a rope -> int
  val concat      : 'a rope * 'a rope -> 'a rope
  val smartConcat : 'a rope * 'a rope -> 'a rope
  val subrope     : 'a rope * int * int -> 'a rope

  val toList      : 'a rope -> 'a list
  val fromList    : 'a list -> 'a rope

  val toString    : 'a rope * ('a -> string) -> string

end
