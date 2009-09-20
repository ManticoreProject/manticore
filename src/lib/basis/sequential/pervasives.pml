(* pervasives.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

datatype option = datatype Option.option

val print = Print.print

val rev = List.rev
val app = List.app
val map = List.map
val foldl = List.foldl
val foldr = List.foldr
val nth = List.nth

fun fst (x, _) = x
fun snd (_, y) = y
